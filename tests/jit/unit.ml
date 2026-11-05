open JIT_opt_lib
module Engine : JIT_engine_lib.JITEngine.OnlineEngine = JIT_orc_lib.OrcEngine
open Test_support

let parse_module s =
  let ctx = Llvm.create_context () in
  let m = Llvm_irreader.parse_ir ctx (Llvm.MemoryBuffer.of_string ~name:"unit" s) in
  ctx, m
;;

let fn_exn m name = Option.get (Llvm.lookup_function name m)

let with_engine ir f =
  let e = Engine.create () in
  Fun.protect
    ~finally:(fun () -> Engine.close e)
    (fun () ->
       Engine.load e ir;
       f e)
;;

(* IR: the file dump; format/payload/make are covered by inline tests. *)
let test_ir () =
  with_tmp_file "" (fun path ->
    IR.dump_to_file path (ir_of "x");
    expect "ir dump" (read_file path = "x\n"))
;;

(* JITOpt: pipeline folding; flag parsing is covered by inline tests. *)
let test_jitopt () =
  let cfg : JITOpt.config = { profile_file = "unused.prof" } in
  let input = fixture "chain.ll" in
  check ~name:"empty run identity" (fun () ->
    if JITOpt.run JITOpt.empty input != input
    then fail "empty pipeline did not return the input");
  let seen = ref [] in
  let out =
    JITOpt.run
      ~dump:(fun n _ -> seen := n :: !seen)
      (JITOpt.of_flags cfg [ "devirt"; "o2" ])
      input
  in
  expect "run dump order" (List.rev !seen = [ "devirt"; "o2" ]);
  expect "run header" (String.starts_with ~prefix:"; opts=devirt,o2\n" (IR.payload out));
  expect "run producer" (contains out.IR.producer "; opts=devirt,o2")
;;

(* Common: payload handling and site detection *)
let test_common_sites () =
  check ~name:"parse bitcode" (fun () ->
    if not (raises_failure (fun () -> Common.parse_payload (IR.make IR.Bitcode "")))
    then fail "bitcode accepted");
  let ctx, m = parse_module (IR.payload (fixture "chain.ll")) in
  let main = fn_exn m "main" in
  let applies =
    Common.instrs_of_fn main |> List.filter (fun i -> Common.is_named i "rukaml_applyN")
  in
  let final = List.nth applies 1 in
  let inter = List.nth applies 0 in
  expect "is_used_as_apply_closure" (Common.is_used_as_apply_closure inter);
  expect "final not used as closure" (not (Common.is_used_as_apply_closure final));
  (match Common.walk final with
   | Some (alloc, args, chain) ->
     expect "walk args" (Array.length args = 2);
     expect "walk chain" (List.length chain = 1);
     expect "walk alloc" (Common.is_named alloc "rukaml_alloc_closure")
   | None -> fail "walk found no chain");
  (match Common.chain_site_of m ~require_const:true ~is_hot:(fun _ -> true) final with
   | Some s -> expect "chain site" (Array.length s.args = 2 && List.length s.chain = 2)
   | None -> fail "chain site not found");
  expect
    "chain site not hot"
    (Option.is_none
       (Common.chain_site_of m ~require_const:true ~is_hot:(fun _ -> false) final));
  check ~name:"target_of" (fun () ->
    match Common.walk final with
    | Some (alloc, _, _) ->
      (match Common.target_of m alloc with
       | Some (fn, 2) ->
         if Llvm.value_name fn <> "f" then fail "target_of = %s" (Llvm.value_name fn)
       | _ -> fail "target_of did not find f/2")
    | None -> fail "walk found no chain");
  check ~name:"extract_callee_name" (fun () ->
    match Common.walk final with
    | Some (alloc, _, _) ->
      let n = Common.extract_callee_name (Llvm.operand alloc 0) in
      if n <> "f" then fail "extract_callee_name = %s" n
    | None -> fail "walk found no chain");
  check ~name:"delete_site_chain" (fun () ->
    match Common.chain_site_of m ~require_const:false ~is_hot:(fun _ -> true) final with
    | Some s ->
      Llvm.replace_all_uses_with s.ins (Llvm.const_int (Llvm.i64_type ctx) 0);
      Common.delete_site_chain s;
      let left =
        List.filter
          (fun i -> Common.is_named i "rukaml_applyN")
          (Common.instrs_of_fn main)
      in
      if left <> [] then fail "%d applyN left after delete" (List.length left)
    | None -> fail "chain site not found");
  let _, dm = parse_module (IR.payload (fixture "direct.ll")) in
  let call_in name =
    Common.instrs_of_fn (fn_exn dm name)
    |> List.find (fun i -> Common.is_op Llvm.Opcode.Call i)
  in
  expect
    "direct site const"
    (Option.is_some
       (Common.direct_site_of
          dm
          ~require_const:true
          ~is_hot:(fun _ -> true)
          (call_in "main")));
  expect
    "direct site non-const rejected"
    (Option.is_none
       (Common.direct_site_of
          dm
          ~require_const:true
          ~is_hot:(fun _ -> true)
          (call_in "h")));
  expect
    "direct site non-const accepted"
    (Option.is_some
       (Common.direct_site_of
          dm
          ~require_const:false
          ~is_hot:(fun _ -> true)
          (call_in "h")));
  expect
    "direct site not a call"
    (Option.is_none
       (Common.direct_site_of
          dm
          ~require_const:false
          ~is_hot:(fun _ -> true)
          (List.hd (List.rev (Common.instrs_of_fn (fn_exn dm "main"))))));
  let _, gm = parse_module (IR.payload (fixture "dynamic.ll")) in
  expect
    "dyn sites"
    (match Common.dyn_sites_of_fn (fn_exn gm "g") with
     | [ s ] -> Array.length s.args = 2 && s.intermediates = []
     | _ -> false);
  expect "defined functions" (List.length (Common.defined_functions dm) = 3);
  expect
    "group_by order"
    (Common.group_by (fun x -> x mod 2) [ 1; 2; 3; 4 ] = [ 1, [ 1; 3 ]; 0, [ 2; 4 ] ]);
  expect "ret type" (Common.ret_type_of ctx (fn_exn m "f") = Some (Llvm.i64_type ctx));
  let vctx, vm = parse_module (IR.payload (fixture "void_ret.ll")) in
  expect
    "ret type void"
    (Common.ret_type_of vctx (fn_exn vm "v") = Some (Llvm.void_type vctx));
  let g = Llvm.define_function "bad" (Llvm.function_type (Llvm.void_type ctx) [||]) m in
  ignore (Llvm.append_block ctx "e" g);
  check ~name:"payload_of rejects invalid IR" (fun () ->
    if not (raises_failure (fun () -> Common.payload_of m))
    then fail "invalid IR accepted");
  let _, zm = parse_module (IR.payload (fixture "apply_noargs.ll")) in
  let zapp =
    List.find
      (fun i -> Common.is_named i "rukaml_applyN")
      (Common.instrs_of_fn (fn_exn zm "z"))
  in
  expect "walk without args" (Option.is_none (Common.walk zapp));
  expect "walk_any without args" (Option.is_none (Common.walk_any zapp));
  expect
    "direct site ignores applyN"
    (Option.is_none
       (Common.direct_site_of zm ~require_const:false ~is_hot:(fun _ -> true) zapp))
;;

(* Common: clone and declaration rewrites *)
let test_common_clone () =
  let ctx, m = parse_module (IR.payload (fixture "chain.ll")) in
  let f = fn_exn m "f" in
  let i64 = Llvm.i64_type ctx in
  (match
     Common.clone_specialized
       ctx
       m
       ~name:"f$spec$0"
       ~fty:(Llvm.function_type i64 [| i64 |])
       ~keep:[| false; true |]
       ~args:[| Llvm.const_int i64 4; Llvm.const_int i64 0 |]
       f
   with
   | Some f' ->
     expect "clone params" (Array.length (Llvm.params f') = 1);
     check ~name:"clone verifies" (fun () ->
       if String.length (Common.payload_of m) = 0 then fail "clone did not verify")
   | None -> fail "clone produced nothing");
  let decl = Llvm.declare_function "ext" (Llvm.function_type i64 [| i64 |]) m in
  expect
    "clone of declaration"
    (Option.is_none
       (Common.clone_specialized
          ctx
          m
          ~name:"ext$spec$0"
          ~fty:(Llvm.function_type i64 [||])
          ~keep:[| false |]
          ~args:[| Llvm.const_int i64 1 |]
          decl));
  let dctx, dm = parse_module (IR.payload (fixture "direct.ll")) in
  let h = fn_exn dm "h" in
  Common.make_declaration dctx h;
  expect "make_declaration" (Llvm.is_declaration h)
;;

(* Profile *)
let test_profile () =
  let cfg : JITOpt.config = { profile_file = "p.prof" } in
  let out = Profile.run cfg (fixture "main5.ll") in
  expect "profile hooks" (contains (IR.payload out) "rukaml_prof_tick");
  expect "profile dump" (contains (IR.payload out) "rukaml_prof_path");
  expect "profile path" (contains (IR.payload out) "p.prof");
  let no_main = Profile.run cfg (ir_of "define i64 @f(i64 %0) {\n  ret i64 %0\n}\n") in
  expect "profile without main" (not (contains (IR.payload no_main) "rukaml_prof_path"))
;;

(* Specialize *)
let test_specialize () =
  check ~name:"read_profile missing" (fun () ->
    if not (raises_failure (fun () -> Specialize.read_profile "/nonexistent/rukaml.prof"))
    then fail "missing profile accepted");
  with_tmp_file "1\tf\n\n4\tg\n" (fun path ->
    expect
      "read_profile entries"
      (List.sort compare (Specialize.read_profile path) = [ "f", 1L; "g", 4L ]));
  with_tmp_file "xx\n" (fun path ->
    check ~name:"read_profile malformed" (fun () ->
      if not (raises_failure (fun () -> Specialize.read_profile path))
      then fail "malformed profile accepted"));
  with_tmp_file "abc\tf\n" (fun path ->
    check ~name:"read_profile bad count" (fun () ->
      if not (raises_failure (fun () -> Specialize.read_profile path))
      then fail "bad count accepted"));
  let ctx, m = parse_module (IR.payload (fixture "chain.ll")) in
  let fresh_state = Specialize.create_state () in
  expect
    "run_on_module cold"
    (not (Specialize.run_on_module ~state:fresh_state ~is_hot:(fun _ -> false) ctx m));
  expect
    "run_on_module no sites"
    (not
       (Specialize.run_on_module
          ~is_hot:(fun _ -> true)
          ctx
          (snd (parse_module (IR.payload (fixture "main5.ll"))))));
  expect
    "run_on_module hot"
    (Specialize.run_on_module ~state:fresh_state ~is_hot:(fun _ -> true) ctx m);
  let p0 = Llvm.param (fn_exn m "f") 0 in
  expect
    "const_key"
    (Specialize.const_key [| Llvm.const_int (Llvm.i64_type ctx) 4; p0 |] = "0=i64 4,")
;;

(* Devirt, O2 *)
let test_devirt_o2 () =
  let out = Devirt.run (fixture "chain.ll") in
  expect "devirt direct call" (contains (IR.payload out) "call i64 @f(");
  expect
    "devirt drops closure"
    (not (contains (IR.payload out) "call i64 @rukaml_alloc_closure"));
  let plain = fixture "direct.ll" in
  check ~name:"devirt no sites" (fun () ->
    if Devirt.run plain != plain then fail "devirt changed a module without sites");
  let _, m = parse_module (IR.payload (fixture "chain.ll")) in
  check ~name:"o2 bogus pipeline" (fun () ->
    if not (raises_failure (fun () -> O2.run_on_module ~passes:"no-such-pass" m))
    then fail "bogus O2 pipeline accepted");
  let o2 = O2.run (fixture "direct.ll") in
  expect "o2 output" (contains (IR.payload o2) "@main(")
;;

(* Tier0: only non-main functions of arity 1..4 get slots. *)
let test_tier0 () =
  let out, meta = Tier0.instrument (fixture "meta.ll") in
  expect
    "tier0 slotted"
    (Array.map (fun (i : Tier0.func_info) -> i.name) meta.funcs = [| "f" |]);
  expect "tier0 sites" (meta.num_sites = 0);
  expect "tier0 threshold" (meta.hot_threshold = Tier0.default_hot_threshold);
  expect "tier0 window" (meta.sample_window = Tier0.default_sample_window);
  expect "tier0 hook" (contains (IR.payload out) "rukaml_jit_hot");
  check ~name:"tier0 run" (fun () ->
    if IR.payload (Tier0.run (fixture "meta.ll")) = IR.payload (fixture "meta.ll")
    then fail "tier0 did not rewrite the payload");
  let _, meta2 =
    Tier0.instrument ~hot_threshold:4L ~sample_window:2L (fixture "meta.ll")
  in
  expect "tier0 custom" (meta2.hot_threshold = 4L && meta2.sample_window = 2L);
  let _, adaptive_meta = Tier0.instrument (fixture "adaptive.ll") in
  expect
    "tier0 counts dynamic sites"
    (Array.fold_left
       (fun n (i : Tier0.func_info) -> n + List.length i.callsites)
       0
       adaptive_meta.funcs
     = 1)
;;

(* Online: delta building *)
let test_online_delta () =
  let master_ir = fixture "rec.ll" in
  let _, meta = Tier0.instrument master_ir in
  let build ?(arg_profile = [||]) ?(site_targets = []) () =
    Online.build_delta
      ~master_ir
      ~meta
      ~func_id:0
      ~ver:1
      ~prev_fallback:"f.t0"
      ~arg_profile
      ~site_targets
  in
  let delta, ver_sym, guarded = build ~arg_profile:[| Some 3L; None |] () in
  expect "delta name" (ver_sym = "f.v1");
  expect "delta defines version" (contains (IR.payload delta) "define i64 @f.v1");
  expect "delta guarded by constant" guarded;
  let _, _, unguarded = build ~arg_profile:[| None; None |] () in
  expect "delta unguarded without constants" (not unguarded);
  (* A monomorphic dynamic site consumes its target through an inline cache. *)
  let adaptive_ir = fixture "adaptive.ll" in
  let _, adaptive_meta = Tier0.instrument adaptive_ir in
  let drive_id =
    let idx = ref (-1) in
    Array.iteri
      (fun i (fi : Tier0.func_info) -> if fi.name = "drive" then idx := i)
      adaptive_meta.funcs;
    !idx
  in
  if drive_id < 0 then fail "drive is not slotted";
  let build_drive ~site_targets =
    Online.build_delta
      ~master_ir:adaptive_ir
      ~meta:adaptive_meta
      ~func_id:drive_id
      ~ver:1
      ~prev_fallback:"drive.t0"
      ~arg_profile:[| None; Some 5L; None |]
      ~site_targets
  in
  let delta, ic_sym, guarded = build_drive ~site_targets:[ 0, ("inc", 1) ] in
  expect "ic delta name" (ic_sym = "drive.v1");
  expect "ic delta guarded" guarded;
  expect "ic checks the cached callee" (contains (IR.payload delta) "icmp eq ptr");
  let delta, _, _ = build_drive ~site_targets:[ 0, ("inc", 2) ] in
  expect "ic arity mismatch ignored" (not (contains (IR.payload delta) "icmp eq ptr"))
;;

(* Engine: lifecycle and error paths *)
let test_engine_lifecycle () =
  expect "engine name" (Engine.name = "orc");
  List.iter
    (fun s -> expect ("runtime symbol " ^ s) (List.mem s Engine.runtime_symbols))
    [ "rukaml_alloc_array"; "rukaml_applyN"; "rukaml_jit_hot"; "rukaml_prof_site" ];
  let e = Engine.create () in
  let e' = Engine.create () in
  Engine.load e' (fixture "main5.ll");
  expect "create twice shares JIT" (Engine.run e "main" = 5L);
  Engine.close e;
  Engine.close e;
  check ~name:"run after close" (fun () ->
    if not (raises_failure (fun () -> Engine.run e "main"))
    then fail "run after close succeeded");
  let e2 = Engine.create () in
  check ~name:"bitcode load rejected" (fun () ->
    if not (raises_failure (fun () -> Engine.load e2 (IR.make IR.Bitcode "")))
    then fail "bitcode accepted");
  check ~name:"bitcode load_delta rejected" (fun () ->
    if not (raises_failure (fun () -> Engine.load_delta e2 (IR.make IR.Bitcode "")))
    then fail "bitcode delta accepted");
  check ~name:"bad IR rejected" (fun () ->
    if not (raises_failure (fun () -> Engine.load e2 (ir_of "not llvm")))
    then fail "invalid IR accepted");
  Engine.load e2 (fixture "nullary.ll");
  expect "run main" (Engine.run e2 "main" = 42L);
  check ~name:"run unknown symbol" (fun () ->
    if not (raises_failure (fun () -> Engine.run e2 "nope"))
    then fail "unknown symbol ran");
  check ~name:"symbol_address unknown" (fun () ->
    if not (raises_failure (fun () -> Engine.symbol_address e2 "nope"))
    then fail "unknown symbol resolved");
  check ~name:"symbol_address main" (fun () ->
    if Engine.symbol_address e2 "main" = 0L then fail "main has no address");
  Engine.close e2
;;

(* Engine: runtime hooks and profiles *)
let test_engine_runtime () =
  with_engine (fixture "runtime_hooks.ll") (fun e ->
    expect "run runtime main" (Engine.run e "main" = 5L);
    check ~name:"hot event" (fun () ->
      match Engine.next_event e ~timeout_ms:1000 with
      | JIT_engine_lib.JITEngine.Hot 3 -> ()
      | _ -> fail "expected Hot 3");
    expect
      "timeout after drain"
      (Engine.next_event e ~timeout_ms:10 = JIT_engine_lib.JITEngine.Timeout);
    expect
      "arg profile"
      (Engine.read_arg_profile e ~func_id:0 ~arity:2 = [| Some 7L; None |]);
    expect
      "arg profile clamp"
      (Array.length (Engine.read_arg_profile e ~func_id:0 ~arity:5) = 4);
    expect
      "arg profile negative"
      (Array.length (Engine.read_arg_profile e ~func_id:0 ~arity:(-1)) = 0);
    expect
      "arg profile out of range"
      (Engine.read_arg_profile e ~func_id:9999 ~arity:2 = [| None; None |]);
    let addr = Engine.symbol_address e "f" in
    expect "site profile" (Engine.read_site_profile e ~site_id:2 = Some (addr, 2, 0));
    expect "site profile unseen" (Engine.read_site_profile e ~site_id:7 = None);
    check ~name:"install without tables" (fun () ->
      if not (raises_failure (fun () -> Engine.install e ~slot:0 ~symbol:"f"))
      then fail "install without tables succeeded");
    expect "read counts without tables" (Engine.read_counts e ~func_id:0 = (0L, 0L));
    Engine.stop_events e;
    expect
      "stop event"
      (Engine.next_event e ~timeout_ms:1000 = JIT_engine_lib.JITEngine.Stop));
  with_engine (fixture "tables.ll") (fun e ->
    Engine.bind_tables e 1;
    expect "run tables main" (Engine.run e "main" = 0L);
    expect "counts after store" (Engine.read_counts e ~func_id:0 = (5L, 0L));
    Engine.reset_func_profile e ~func_id:0;
    expect "counts after reset" (Engine.read_counts e ~func_id:0 = (0L, 0L));
    Engine.install e ~slot:0 ~symbol:"f";
    check ~name:"install out of range" (fun () ->
      if not (raises_failure (fun () -> Engine.install e ~slot:3 ~symbol:"f"))
      then fail "out-of-range slot accepted");
    check ~name:"install unknown" (fun () ->
      if not (raises_failure (fun () -> Engine.install e ~slot:0 ~symbol:"nope"))
      then fail "unknown symbol installed");
    Engine.bind_tables e 0);
  with_engine (fixture "runtime_hooks.ll") (fun e ->
    check ~name:"bind without globals" (fun () ->
      if not (raises_failure (fun () -> Engine.bind_tables e 1))
      then fail "bind without tables succeeded"))
;;

(* Engine: delta loading *)
let test_engine_delta () =
  with_engine (fixture "nullary.ll") (fun e ->
    Engine.load_delta e (ir_of "define i64 @d.v1(i64 %0) {\n  ret i64 %0\n}\n");
    check ~name:"delta symbol" (fun () ->
      if Engine.symbol_address e "d.v1" = 0L then fail "delta symbol not resolved"))
;;

(* Online: end-to-end runner *)
let test_run_online () =
  let result =
    Online.run_online
      (module Engine)
      ~hot_threshold:4L
      ~sample_window:2L
      ~timeout_ms:5
      (fixture "rec.ll")
  in
  expect "run_online result" (result = 5050L);
  let adaptive =
    Online.run_online
      (module Engine)
      ~hot_threshold:4L
      ~sample_window:2L
      ~timeout_ms:1
      (fixture "adaptive.ll")
  in
  expect "run_online adaptive result" (adaptive = 900030000L);
  with_tmp_dir (fun dir ->
    let result2 =
      Online.run_online
        (module Engine)
        ~hot_threshold:4L
        ~sample_window:2L
        ~timeout_ms:5
        ~dump_dir:dir
        (fixture "rec.ll")
    in
    expect "run_online dump result" (result2 = 5050L);
    expect "run_online input dump" (Sys.file_exists (Filename.concat dir "00-input.ll"));
    expect "run_online tier0 dump" (Sys.file_exists (Filename.concat dir "01-tier0.ll")))
;;

let () =
  check ~name:"ir" test_ir;
  check ~name:"jitopt" test_jitopt;
  check ~name:"common_sites" test_common_sites;
  check ~name:"common_clone" test_common_clone;
  check ~name:"profile" test_profile;
  check ~name:"specialize" test_specialize;
  check ~name:"devirt_o2" test_devirt_o2;
  check ~name:"tier0" test_tier0;
  check ~name:"online_delta" test_online_delta;
  check ~name:"engine_lifecycle" test_engine_lifecycle;
  check ~name:"engine_runtime" test_engine_runtime;
  check ~name:"engine_delta" test_engine_delta;
  check ~name:"run_online" test_run_online;
  summary ~suite:"unit" ()
;;
