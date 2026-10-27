open JIT_opt_lib
module Engine : JIT_engine_lib.JITEngine.OnlineEngine = JIT_orc_lib.OrcEngine

let failures = ref 0

let check name cond =
  if not cond
  then (
    incr failures;
    Printf.eprintf "FAIL: %s\n%!" name)
;;

let raises_failure f =
  match f () with
  | _ -> false
  | exception Failure _ -> true
;;

let contains hay needle =
  let nh = String.length hay
  and nn = String.length needle in
  let rec loop i = i + nn <= nh && (String.sub hay i nn = needle || loop (i + 1)) in
  loop 0
;;

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path data =
  Out_channel.with_open_bin path (fun oc -> output_string oc data)
;;

let with_tmp_file ?(suffix = "") contents f =
  let path = Filename.temp_file "rukaml_unit" suffix in
  write_file path contents;
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () -> f path)
;;

let ir_of s = IR.make IR.TextLL s

let parse_module s =
  let ctx = Llvm.create_context () in
  let m = Llvm_irreader.parse_ir ctx (Llvm.MemoryBuffer.of_string ~name:"unit" s) in
  ctx, m
;;

let fn_exn m name = Option.get (Llvm.lookup_function name m)

let chain_src =
  {|
declare i64 @rukaml_alloc_closure(i64, i64)
declare i64 @rukaml_applyN(i64, i64, ...)
define i64 @f(i64 %0, i64 %1) {
  %r = add i64 %0, %1
  ret i64 %r
}
define i64 @main() {
  %cl = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @f to i64), i64 2)
  %p = call i64 (i64, i64, ...) @rukaml_applyN(i64 %cl, i64 1, i64 4)
  %r = call i64 (i64, i64, ...) @rukaml_applyN(i64 %p, i64 1, i64 7)
  ret i64 %r
}
|}
;;

let direct_src =
  {|
define i64 @f(i64 %0, i64 %1) {
  %r = add i64 %0, %1
  ret i64 %r
}
define i64 @h(i64 %a, i64 %b) {
  %r = call i64 @f(i64 %a, i64 %b)
  ret i64 %r
}
define i64 @main() {
  %r = call i64 @f(i64 1, i64 2)
  ret i64 %r
}
|}
;;

let dyn_src =
  {|
declare i64 @rukaml_applyN(i64, i64, ...)
define i64 @g(i64 %c) {
  %r = call i64 (i64, i64, ...) @rukaml_applyN(i64 %c, i64 2, i64 1, i64 2)
  ret i64 %r
}
|}
;;

let rec_src =
  {|
declare i64 @rukaml_alloc_closure(i64, i64)
declare i64 @rukaml_applyN(i64, i64, ...)
define i64 @f(i64 %0, i64 %1) {
entry:
  %c = icmp eq i64 %0, 0
  br i1 %c, label %done, label %rec
rec:
  %n = sub i64 %0, 1
  %a = add i64 %1, %0
  %r = call i64 @f(i64 %n, i64 %a)
  ret i64 %r
done:
  ret i64 %1
}
define i64 @main() {
  %r = call i64 @f(i64 100, i64 0)
  ret i64 %r
}
|}
;;

let adaptive_src =
  {|
declare i64 @rukaml_alloc_closure(i64, i64)
declare i64 @rukaml_applyN(i64, i64, ...)
define i64 @inc(i64 %0) {
  %r = add i64 %0, 1
  ret i64 %r
}
define i64 @dec(i64 %0) {
  %r = sub i64 %0, 1
  ret i64 %r
}
define i64 @drive(i64 %k, i64 %n, i64 %acc) {
entry:
  %c = icmp eq i64 %n, 0
  br i1 %c, label %done, label %rec
rec:
  %p = call i64 (i64, i64, ...) @rukaml_applyN(i64 %k, i64 1, i64 %n)
  %a = add i64 %acc, %p
  %m = sub i64 %n, 1
  %r = call i64 @drive(i64 %k, i64 %m, i64 %a)
  ret i64 %r
done:
  ret i64 %acc
}
define i64 @main() {
  %ci = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @inc to i64), i64 1)
  %cd = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @dec to i64), i64 1)
  %a = call i64 @drive(i64 %ci, i64 30000, i64 0)
  %b = call i64 @drive(i64 %cd, i64 30000, i64 %a)
  ret i64 %b
}
|}
;;

let runtime_src =
  {|
declare void @rukaml_jit_hot(i64)
declare void @rukaml_prof_args(i64, i64, i64, i64, i64)
declare void @rukaml_prof_site(i64, ptr)
declare i64 @rukaml_alloc_closure(i64, i64)
define i64 @f(i64 %0, i64 %1) {
  %r = add i64 %0, %1
  ret i64 %r
}
define i64 @main() {
  call void @rukaml_jit_hot(i64 3)
  call void @rukaml_prof_args(i64 0, i64 7, i64 8, i64 9, i64 10)
  call void @rukaml_prof_args(i64 0, i64 7, i64 99, i64 9, i64 10)
  %cl = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @f to i64), i64 2)
  %p = inttoptr i64 %cl to ptr
  call void @rukaml_prof_site(i64 2, ptr %p)
  ret i64 5
}
|}
;;

let tables_src =
  {|
@rukaml.slots = global [1 x ptr] zeroinitializer
@rukaml.cnt = global [1 x i64] zeroinitializer
@rukaml.miss = global [1 x i64] zeroinitializer
define i64 @main() {
  store i64 5, ptr @rukaml.cnt
  ret i64 0
}
define i64 @f(i64 %0) {
  ret i64 %0
}
|}
;;

let with_engine src f =
  let e = Engine.create () in
  Fun.protect
    ~finally:(fun () -> Engine.close e)
    (fun () ->
       Engine.load e (ir_of src);
       f e)
;;

(* IR *)
let () =
  let t = ir_of "x" in
  check "ir format" (IR.format t = IR.TextLL);
  check "ir payload" (IR.payload t = "x");
  check "ir producer" (t.IR.producer = IR.producer);
  with_tmp_file "" (fun path ->
    IR.dump_to_file path t;
    check "ir dump" (read_file path = "x\n"))
;;

(* JITOpt *)
let cfg_for path : JITOpt.config = { profile_file = path }
let cfg = cfg_for "unused.prof"

let () =
  let p = JITOpt.of_flags cfg [ "devirt,o2"; "tier0" ] in
  check "of_flags comma" (JITOpt.names p = [ "devirt"; "o2"; "tier0" ]);
  check "of_flags unknown" (raises_failure (fun () -> JITOpt.of_flags cfg [ "online" ]));
  check
    "requires tier0"
    (JITOpt.requires (JITOpt.of_flags cfg [ "tier0" ]) = Tier0.requires);
  check
    "missing_runtime"
    (JITOpt.missing_runtime ~available:[ "a"; "b" ] [ "b"; "c" ] = [ "c" ]);
  check "empty pipeline names" (JITOpt.names JITOpt.empty = []);
  let input = ir_of chain_src in
  check "empty run identity" (JITOpt.run JITOpt.empty input == input);
  let seen = ref [] in
  let out =
    JITOpt.run
      ~dump:(fun n _ -> seen := n :: !seen)
      (JITOpt.of_flags cfg [ "devirt"; "o2" ])
      input
  in
  check "run dump order" (List.rev !seen = [ "devirt"; "o2" ]);
  check "run header" (String.starts_with ~prefix:"; opts=devirt,o2\n" (IR.payload out));
  check "run producer" (contains out.IR.producer "; opts=devirt,o2")
;;

(* Common: payload handling and site detection *)
let () =
  check
    "parse bitcode"
    (raises_failure (fun () -> Common.parse_payload (IR.make IR.Bitcode "")));
  let ctx, m = parse_module chain_src in
  let main = fn_exn m "main" in
  let applies =
    Common.instrs_of_fn main |> List.filter (fun i -> Common.is_named i "rukaml_applyN")
  in
  let final = List.nth applies 1 in
  let inter = List.nth applies 0 in
  check "is_used_as_apply_closure" (Common.is_used_as_apply_closure inter);
  check "final not used as closure" (not (Common.is_used_as_apply_closure final));
  (match Common.walk final with
   | Some (alloc, args, chain) ->
     check "walk args" (Array.length args = 2);
     check "walk chain" (List.length chain = 1);
     check "walk alloc" (Common.is_named alloc "rukaml_alloc_closure")
   | None -> check "walk found chain" false);
  (match Common.chain_site_of m ~require_const:true ~is_hot:(fun _ -> true) final with
   | Some s -> check "chain site" (Array.length s.args = 2 && List.length s.chain = 2)
   | None -> check "chain site found" false);
  check
    "chain site not hot"
    (Option.is_none
       (Common.chain_site_of m ~require_const:true ~is_hot:(fun _ -> false) final));
  check
    "target_of"
    (match Common.walk final with
     | Some (alloc, _, _) ->
       (match Common.target_of m alloc with
        | Some (fn, 2) -> Llvm.value_name fn = "f"
        | _ -> false)
     | None -> false);
  (match Common.walk final with
   | Some (alloc, _, _) ->
     check "extract_callee_name" (Common.extract_callee_name (Llvm.operand alloc 0) = "f")
   | None -> check "extract_callee_name walk" false);
  (match Common.chain_site_of m ~require_const:false ~is_hot:(fun _ -> true) final with
   | Some s ->
     Llvm.replace_all_uses_with s.ins (Llvm.const_int (Llvm.i64_type ctx) 0);
     Common.delete_site_chain s;
     check
       "delete_site_chain removes chain"
       (List.length
          (List.filter
             (fun i -> Common.is_named i "rukaml_applyN")
             (Common.instrs_of_fn main))
        = 0)
   | None -> check "delete_site_chain site" false);
  let _, dm = parse_module direct_src in
  let call_in name =
    Common.instrs_of_fn (fn_exn dm name)
    |> List.find (fun i -> Common.is_op Llvm.Opcode.Call i)
  in
  check
    "direct site const"
    (Option.is_some
       (Common.direct_site_of
          dm
          ~require_const:true
          ~is_hot:(fun _ -> true)
          (call_in "main")));
  check
    "direct site non-const rejected"
    (Option.is_none
       (Common.direct_site_of
          dm
          ~require_const:true
          ~is_hot:(fun _ -> true)
          (call_in "h")));
  check
    "direct site non-const accepted"
    (Option.is_some
       (Common.direct_site_of
          dm
          ~require_const:false
          ~is_hot:(fun _ -> true)
          (call_in "h")));
  check
    "direct site not a call"
    (Option.is_none
       (Common.direct_site_of
          dm
          ~require_const:false
          ~is_hot:(fun _ -> true)
          (List.hd (List.rev (Common.instrs_of_fn (fn_exn dm "main"))))));
  let _, gm = parse_module dyn_src in
  check
    "dyn sites"
    (match Common.dyn_sites_of_fn (fn_exn gm "g") with
     | [ s ] -> Array.length s.args = 2 && s.intermediates = []
     | _ -> false);
  check "defined functions" (List.length (Common.defined_functions dm) = 3);
  check
    "group_by order"
    (Common.group_by (fun x -> x mod 2) [ 1; 2; 3; 4 ] = [ 1, [ 1; 3 ]; 0, [ 2; 4 ] ]);
  check "ret type" (Common.ret_type_of ctx (fn_exn m "f") = Some (Llvm.i64_type ctx));
  let g = Llvm.define_function "bad" (Llvm.function_type (Llvm.void_type ctx) [||]) m in
  ignore (Llvm.append_block ctx "e" g);
  check "payload_of rejects invalid IR" (raises_failure (fun () -> Common.payload_of m));
  let vctx, vm = parse_module "define void @v() {\n  ret void\n}\n" in
  check
    "ret type void"
    (Common.ret_type_of vctx (fn_exn vm "v") = Some (Llvm.void_type vctx));
  let _, zm =
    parse_module
      "declare i64 @rukaml_applyN(i64, i64, ...)\n\
       define i64 @z(i64 %c) {\n\
      \  %r = call i64 (i64, i64, ...) @rukaml_applyN(i64 %c, i64 0)\n\
      \  ret i64 %r\n\
       }\n"
  in
  let zapp =
    List.find
      (fun i -> Common.is_named i "rukaml_applyN")
      (Common.instrs_of_fn (fn_exn zm "z"))
  in
  check "walk without args" (Option.is_none (Common.walk zapp));
  check "walk_any without args" (Option.is_none (Common.walk_any zapp));
  check
    "direct site ignores applyN"
    (Option.is_none
       (Common.direct_site_of zm ~require_const:false ~is_hot:(fun _ -> true) zapp))
;;

(* Common: clone and declaration rewrites *)
let () =
  let ctx, m = parse_module chain_src in
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
     check "clone params" (Array.length (Llvm.params f') = 1);
     check "clone verifies" (String.length (Common.payload_of m) > 0)
   | None -> check "clone produced" false);
  let decl = Llvm.declare_function "ext" (Llvm.function_type i64 [| i64 |]) m in
  check
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
  let dctx, dm = parse_module direct_src in
  let h = fn_exn dm "h" in
  Common.make_declaration dctx h;
  check "make_declaration" (Llvm.is_declaration h)
;;

(* Profile *)
let () =
  let out =
    Profile.run (cfg_for "p.prof") (ir_of "define i64 @main() {\n  ret i64 1\n}\n")
  in
  check "profile hooks" (contains (IR.payload out) "rukaml_prof_tick");
  check "profile dump" (contains (IR.payload out) "rukaml_prof_path");
  check "profile path" (contains (IR.payload out) "p.prof");
  let no_main =
    Profile.run (cfg_for "p.prof") (ir_of "define i64 @f(i64 %0) {\n  ret i64 %0\n}\n")
  in
  check "profile without main" (not (contains (IR.payload no_main) "rukaml_prof_path"))
;;

(* Specialize *)
let () =
  check
    "read_profile missing"
    (raises_failure (fun () -> Specialize.read_profile "/nonexistent/rukaml.prof"));
  with_tmp_file "1\tf\n\n4\tg\n" (fun path ->
    check
      "read_profile entries"
      (List.sort compare (Specialize.read_profile path) = [ "f", 1L; "g", 4L ]));
  with_tmp_file "xx\n" (fun path ->
    check
      "read_profile malformed"
      (raises_failure (fun () -> Specialize.read_profile path)));
  with_tmp_file "abc\tf\n" (fun path ->
    check
      "read_profile bad count"
      (raises_failure (fun () -> Specialize.read_profile path)));
  let ctx, m = parse_module chain_src in
  let fresh_state = Specialize.create_state () in
  check
    "run_on_module cold"
    (not (Specialize.run_on_module ~state:fresh_state ~is_hot:(fun _ -> false) ctx m));
  check
    "run_on_module no sites"
    (not
       (Specialize.run_on_module
          ~is_hot:(fun _ -> true)
          ctx
          (snd (parse_module "define i64 @main() {\n  ret i64 1\n}\n"))));
  check
    "run_on_module hot"
    (Specialize.run_on_module ~state:fresh_state ~is_hot:(fun _ -> true) ctx m);
  let p0 = Llvm.param (fn_exn m "f") 0 in
  check
    "const_key"
    (Specialize.const_key [| Llvm.const_int (Llvm.i64_type ctx) 4; p0 |] = "0=i64 4,")
;;

(* Devirt, O2 *)
let () =
  let out = Devirt.run (ir_of chain_src) in
  check "devirt direct call" (contains (IR.payload out) "call i64 @f(");
  check
    "devirt drops closure"
    (not (contains (IR.payload out) "call i64 @rukaml_alloc_closure"));
  let plain = ir_of direct_src in
  check "devirt no sites" (Devirt.run plain == plain);
  let _, m = parse_module chain_src in
  check
    "o2 bogus pipeline"
    (raises_failure (fun () -> O2.run_on_module ~passes:"no-such-pass" m));
  let o2 = O2.run (ir_of direct_src) in
  check "o2 output" (contains (IR.payload o2) "@main(")
;;

(* Tier0 *)
let () =
  let meta_src =
    {|
define i64 @f(i64 %0, i64 %1) {
  %r = add i64 %0, %1
  ret i64 %r
}
define i64 @g(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4) {
  ret i64 %0
}
define i64 @h() {
  ret i64 1
}
define i64 @main() {
  %r = call i64 @f(i64 1, i64 2)
  ret i64 %r
}
|}
  in
  let out, meta = Tier0.instrument (ir_of meta_src) in
  check
    "tier0 slotted"
    (Array.map (fun (i : Tier0.func_info) -> i.name) meta.funcs = [| "f" |]);
  check "tier0 threshold" (meta.hot_threshold = Tier0.default_hot_threshold);
  check "tier0 window" (meta.sample_window = Tier0.default_sample_window);
  check "tier0 hook" (contains (IR.payload out) "rukaml_jit_hot");
  check "tier0 run" (IR.payload (Tier0.run (ir_of meta_src)) <> meta_src);
  let _, meta2 = Tier0.instrument ~hot_threshold:4L ~sample_window:2L (ir_of meta_src) in
  check "tier0 custom" (meta2.hot_threshold = 4L && meta2.sample_window = 2L)
;;

(* Online: delta building *)
let () =
  let _, meta = Tier0.instrument (ir_of rec_src) in
  let master_ir = ir_of rec_src in
  let build arg_profile =
    Online.build_delta
      ~master_ir
      ~meta
      ~func_id:0
      ~ver:1
      ~prev_fallback:"f.t0"
      ~arg_profile
      ~site_targets:[]
  in
  let delta, ver_sym, guarded = build [| Some 3L; None |] in
  check "delta name" (ver_sym = "f.v1");
  check "delta defines version" (contains (IR.payload delta) "define i64 @f.v1");
  check "delta guarded by constant" guarded;
  let _, _, unguarded = build [| None; None |] in
  check "delta unguarded without constants" (not unguarded)
;;

(* Engine: lifecycle and error paths *)
let () =
  check "engine name" (Engine.name = "orc");
  List.iter
    (fun s -> check ("runtime symbol " ^ s) (List.mem s Engine.runtime_symbols))
    [ "rukaml_alloc_array"; "rukaml_applyN"; "rukaml_jit_hot"; "rukaml_prof_site" ];
  let e = Engine.create () in
  let e' = Engine.create () in
  Engine.load e' (ir_of "define i64 @main() {\n  ret i64 3\n}\n");
  check "create twice shares JIT" (Engine.run e "main" = 3L);
  Engine.close e;
  Engine.close e;
  check "run after close" (raises_failure (fun () -> Engine.run e "main"));
  let e2 = Engine.create () in
  check
    "bitcode load rejected"
    (raises_failure (fun () -> Engine.load e2 (IR.make IR.Bitcode "")));
  check
    "bitcode load_delta rejected"
    (raises_failure (fun () -> Engine.load_delta e2 (IR.make IR.Bitcode "")));
  check "bad IR rejected" (raises_failure (fun () -> Engine.load e2 (ir_of "not llvm")));
  Engine.load e2 (ir_of "define i64 @main() {\n  ret i64 9\n}\n");
  check "run main" (Engine.run e2 "main" = 9L);
  check "run unknown symbol" (raises_failure (fun () -> Engine.run e2 "nope"));
  check
    "symbol_address unknown"
    (raises_failure (fun () -> Engine.symbol_address e2 "nope"));
  check "symbol_address main" (Engine.symbol_address e2 "main" <> 0L);
  Engine.close e2
;;

(* Engine: runtime hooks and profiles *)
let () =
  with_engine runtime_src (fun e ->
    check "run runtime main" (Engine.run e "main" = 5L);
    (match Engine.next_event e ~timeout_ms:1000 with
     | JIT_engine_lib.JITEngine.Hot 3 -> ()
     | _ -> check "hot event" false);
    check
      "timeout after drain"
      (Engine.next_event e ~timeout_ms:10 = JIT_engine_lib.JITEngine.Timeout);
    check
      "arg profile"
      (Engine.read_arg_profile e ~func_id:0 ~arity:2 = [| Some 7L; None |]);
    check
      "arg profile clamp"
      (Array.length (Engine.read_arg_profile e ~func_id:0 ~arity:5) = 4);
    check
      "arg profile negative"
      (Array.length (Engine.read_arg_profile e ~func_id:0 ~arity:(-1)) = 0);
    check
      "arg profile out of range"
      (Engine.read_arg_profile e ~func_id:9999 ~arity:2 = [| None; None |]);
    let addr = Engine.symbol_address e "f" in
    check "site profile" (Engine.read_site_profile e ~site_id:2 = Some (addr, 2, 0));
    check "site profile unseen" (Engine.read_site_profile e ~site_id:7 = None);
    check
      "install without tables"
      (raises_failure (fun () -> Engine.install e ~slot:0 ~symbol:"f"));
    check "read counts without tables" (Engine.read_counts e ~func_id:0 = (0L, 0L));
    Engine.stop_events e;
    check
      "stop event"
      (Engine.next_event e ~timeout_ms:1000 = JIT_engine_lib.JITEngine.Stop));
  with_engine tables_src (fun e ->
    Engine.bind_tables e 1;
    check "run tables main" (Engine.run e "main" = 0L);
    check "counts after store" (Engine.read_counts e ~func_id:0 = (5L, 0L));
    Engine.reset_func_profile e ~func_id:0;
    check "counts after reset" (Engine.read_counts e ~func_id:0 = (0L, 0L));
    Engine.install e ~slot:0 ~symbol:"f";
    check
      "install out of range"
      (raises_failure (fun () -> Engine.install e ~slot:3 ~symbol:"f"));
    check
      "install unknown"
      (raises_failure (fun () -> Engine.install e ~slot:0 ~symbol:"nope"));
    Engine.bind_tables e 0);
  with_engine runtime_src (fun e ->
    check "bind without globals" (raises_failure (fun () -> Engine.bind_tables e 1)))
;;

(* Engine: delta loading *)
let () =
  let e = Engine.create () in
  Fun.protect
    ~finally:(fun () -> Engine.close e)
    (fun () ->
       Engine.load e (ir_of "define i64 @main() {\n  ret i64 0\n}\n");
       Engine.load_delta e (ir_of "define i64 @d.v1(i64 %0) {\n  ret i64 %0\n}\n");
       check "delta symbol" (Engine.symbol_address e "d.v1" <> 0L))
;;

(* Online: end-to-end runner *)
let () =
  let result =
    Online.run_online
      (module Engine)
      ~hot_threshold:4L
      ~sample_window:2L
      ~timeout_ms:5
      (ir_of rec_src)
  in
  check "run_online result" (result = 5050L);
  let adaptive =
    Online.run_online
      (module Engine)
      ~hot_threshold:4L
      ~sample_window:2L
      ~timeout_ms:1
      (ir_of adaptive_src)
  in
  check "run_online adaptive result" (adaptive = 900030000L);
  let dir = Filename.temp_file "rukaml_online" "" in
  Sys.remove dir;
  let result2 =
    Online.run_online
      (module Engine)
      ~hot_threshold:4L
      ~sample_window:2L
      ~timeout_ms:5
      ~dump_dir:dir
      (ir_of rec_src)
  in
  check "run_online dump result" (result2 = 5050L);
  check "run_online input dump" (Sys.file_exists (Filename.concat dir "00-input.ll"));
  check "run_online tier0 dump" (Sys.file_exists (Filename.concat dir "01-tier0.ll"));
  Array.iter (fun f -> Sys.remove (Filename.concat dir f)) (Sys.readdir dir);
  Sys.rmdir dir
;;

let () =
  if !failures > 0
  then (
    Printf.eprintf "unit: %d failure(s)\n" !failures;
    exit 1)
  else print_endline "unit OK"
;;
