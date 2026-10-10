(* Lifecycle selfcheck for the JIT_engine_lib.JITEngine.BackEngine
   contract in its ORC implementation, plus the optimization pipeline in
   JIT_opt_lib. Run from the alias runtest (jit/selfcheck/dune). Checks,
   within a single process:
   1. a second create without close does not duplicate runtime symbols
      (registration happens once, under "just created", in
      rukaml_orc_create);
   2. load/run/close; a repeated close is safe (delete nullptr);
   3. create after close yields a fresh engine (main is defined again);
   4. run after close raises Failure;
   5. an empty pipeline is the identity: payload and producer stay
      byte-for-byte;
   6. an unknown pass name fails with Failure;
   7. missing_runtime reports exactly the unmet requirements, and the
      engine supplies what the profile pass needs;
   8. the profile pass instruments a payload, the engine runs it, and the
      profile file gets a non-zero counter;
   9. the specialize pass collapses a hot curried call chain into a
      direct call (hot: payload has a clone and the program still runs;
      cold: payload untouched apart from the opts comment);
  10. a hot chain whose result feeds another site's argument
      ([f 100 (f 10 x)]) specializes without leaving a dangling
      instruction reference.

   The base payload is a hand-written [IR.TextLL] with no runtime
   references, so the lifecycle tests do not depend on the LLVM
   backend. *)
module Engine : JIT_engine_lib.JITEngine.BackEngine = JIT_orc_lib.OrcEngine
module JITOpt = JIT_opt_lib.JITOpt

let payload = "define i64 @main() {\n  ret i64 5\n}\n"

let fail fmt =
  Printf.ksprintf
    (fun s ->
       prerr_endline ("selfcheck FAIL: " ^ s);
       exit 1)
    fmt
;;

let expect_main tag v = if v <> 5L then fail "%s: main = %Ld, expected 5L" tag v

let contains hay needle =
  let nh = String.length hay
  and nn = String.length needle in
  let rec loop i = i + nn <= nh && (String.sub hay i nn = needle || loop (i + 1)) in
  loop 0
;;

let drop_first_line s =
  match String.index_opt s '\n' with
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  | None -> s
;;

let write_file path data =
  let oc = open_out path in
  output_string oc data;
  close_out oc
;;

let read_file path =
  let ic = open_in path in
  let data = really_input_string ic (in_channel_length ic) in
  close_in ic;
  data
;;

let with_profile contents f =
  let path = Filename.temp_file "rukaml_selfcheck" ".prof" in
  write_file path contents;
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () -> f path)
;;

let () =
  let ir = IR.make IR.TextLL payload in
  let e1 = Engine.create () in
  let _e2 = Engine.create () in
  Engine.load e1 ir;
  expect_main "first run" (Engine.run e1 "main");
  Engine.close e1;
  Engine.close _e2;
  let e3 = Engine.create () in
  Engine.load e3 ir;
  expect_main "second run" (Engine.run e3 "main");
  Engine.close e3;
  (match Engine.run e3 "main" with
   | v -> fail "run after close returned %Ld, expected Failure" v
   | exception Failure msg -> if String.trim msg = "" then fail "empty Failure");
  (* 5. empty pipeline: identity *)
  let ir' = JITOpt.run JITOpt.empty ir in
  if IR.payload ir' <> payload || ir'.IR.producer <> ir.IR.producer
  then fail "empty pipeline changed payload or producer";
  (* 6. unknown pass name *)
  let config : JITOpt.config = { profile_file = "unused.prof" } in
  (match JITOpt.of_flags config [ "nope" ] with
   | _ -> fail "unknown pass name accepted"
   | exception Failure msg -> if String.length msg = 0 then fail "empty Failure");
  (* 7. requirement checking *)
  let profile_pipeline = JITOpt.of_flags config [ "profile" ] in
  if JITOpt.names profile_pipeline <> [ "profile" ] then fail "wrong pipeline names";
  if JITOpt.requires profile_pipeline <> [ "rukaml_prof_tick"; "rukaml_prof_dump" ]
  then fail "wrong profile requirements";
  let missing = JITOpt.missing_runtime ~available:[ "a"; "b" ] [ "b"; "c"; "d" ] in
  if missing <> [ "c"; "d" ]
  then fail "missing_runtime = [%s], expected [c, d]" (String.concat ", " missing);
  let missing_engine =
    JITOpt.missing_runtime
      ~available:Engine.runtime_symbols
      (JITOpt.requires profile_pipeline)
  in
  if missing_engine <> []
  then fail "engine lacks runtime symbols: [%s]" (String.concat ", " missing_engine);
  (* 8. profile pass end to end *)
  with_profile "" (fun prof_path ->
    let prof_config : JITOpt.config = { profile_file = prof_path } in
    let prof_ir = JITOpt.run (JITOpt.of_flags prof_config [ "profile" ]) ir in
    if String.length (IR.payload prof_ir) <= String.length payload
    then fail "profile pass did not instrument the payload";
    let e = Engine.create () in
    Engine.load e prof_ir;
    expect_main "profile run" (Engine.run e "main");
    Engine.close e;
    let content = read_file prof_path in
    if not (contains content "1\tmain")
    then fail "profile file %S lacks a main counter" content);
  (* 9. specialize pass end to end *)
  let chain_payload =
    "declare i64 @rukaml_alloc_closure(i64, i64)\n\
     declare i64 @rukaml_applyN(i64, i64, ...)\n\
     define i64 @f(i64 %0, i64 %1) {\n\
    \  %r = add i64 %0, %1\n\
    \  ret i64 %r\n\
     }\n\
     define i64 @main() {\n\
    \  %cl = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @f to i64), i64 2)\n\
    \  %p1 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %cl, i64 1, i64 4)\n\
    \  %r = call i64 (i64, i64, ...) @rukaml_applyN(i64 %p1, i64 1, i64 7)\n\
    \  ret i64 %r\n\
     }\n"
  in
  with_profile "1\tf\n" (fun cold_path ->
    let cold_config : JITOpt.config = { profile_file = cold_path } in
    let cold_ir =
      JITOpt.run
        (JITOpt.of_flags cold_config [ "specialize" ])
        (IR.make IR.TextLL chain_payload)
    in
    if drop_first_line (IR.payload cold_ir) <> chain_payload
    then fail "cold specialize changed the payload");
  with_profile "4\tf\n" (fun hot_path ->
    let hot_config : JITOpt.config = { profile_file = hot_path } in
    let hot_ir =
      JITOpt.run
        (JITOpt.of_flags hot_config [ "specialize" ])
        (IR.make IR.TextLL chain_payload)
    in
    if not (contains (IR.payload hot_ir) "$spec$")
    then fail "hot specialize produced no clone";
    let e = Engine.create () in
    Engine.load e hot_ir;
    (match Engine.run e "main" with
     | 11L -> ()
     | v -> fail "specialized main = %Ld, expected 11L" v);
    Engine.close e);
  (* 10. specialize when a rewritten site's result feeds another site *)
  let chained_payload =
    "declare i64 @rukaml_alloc_closure(i64, i64)\n\
     declare i64 @rukaml_applyN(i64, i64, ...)\n\
     define i64 @f(i64 %0, i64 %1) {\n\
    \  %r = add i64 %0, %1\n\
    \  ret i64 %r\n\
     }\n\
     define i64 @main() {\n\
    \  %c1 = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @f to i64), i64 2)\n\
    \  %p1 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %c1, i64 1, i64 10)\n\
    \  %r1 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %p1, i64 1, i64 5)\n\
    \  %c2 = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @f to i64), i64 2)\n\
    \  %p2 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %c2, i64 1, i64 100)\n\
    \  %r2 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %p2, i64 1, i64 %r1)\n\
    \  ret i64 %r2\n\
     }\n"
  in
  with_profile "4\tf\n" (fun chain_path ->
    let chain_config : JITOpt.config = { profile_file = chain_path } in
    let chain_ir =
      try
        JITOpt.run
          (JITOpt.of_flags chain_config [ "specialize" ])
          (IR.make IR.TextLL chained_payload)
      with
      | Failure msg -> fail "chained specialize: %s" msg
    in
    let e = Engine.create () in
    Engine.load e chain_ir;
    (match Engine.run e "main" with
     | 115L -> ()
     | v -> fail "chained specialized main = %Ld, expected 115L" v);
    Engine.close e);
  print_endline "selfcheck OK"
;;
