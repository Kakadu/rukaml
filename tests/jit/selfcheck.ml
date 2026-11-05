module Engine : JIT_engine_lib.JITEngine.BackEngine = JIT_orc_lib.OrcEngine
module JITOpt = JIT_opt_lib.JITOpt
open Test_support

let expect_main tag v = if v <> 5L then fail "%s: main = %Ld, expected 5L" tag v

let drop_first_line s =
  match String.index_opt s '\n' with
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  | None -> s
;;

let payload = fixture "main5.ll"
let chain_payload = fixture "chain.ll"

let run_and_close ir =
  let e = Engine.create () in
  Fun.protect
    ~finally:(fun () -> Engine.close e)
    (fun () ->
       Engine.load e ir;
       Engine.run e "main")
;;

let test_lifecycle () =
  let e1 = Engine.create () in
  Engine.load e1 payload;
  expect_main "first run" (Engine.run e1 "main");
  Engine.close e1;
  Engine.close e1 (* repeated close is safe *);
  let e2 = Engine.create () in
  Engine.load e2 payload;
  expect_main "second run" (Engine.run e2 "main");
  Engine.close e2;
  match Engine.run e2 "main" with
  | v -> fail "run after close returned %Ld, expected Failure" v
  | exception Failure _ -> ()
;;

let test_empty_pipeline () =
  let ir' = JITOpt.run JITOpt.empty payload in
  if IR.payload ir' <> IR.payload payload || ir'.IR.producer <> payload.IR.producer
  then fail "empty pipeline changed payload or producer"
;;

let test_unknown_pass () =
  let config : JITOpt.config = { profile_file = "unused.prof" } in
  match JITOpt.of_flags config [ "nope" ] with
  | _ -> fail "unknown pass name accepted"
  | exception Failure _ -> ()
;;

let test_missing_runtime () =
  let config : JITOpt.config = { profile_file = "unused.prof" } in
  let missing = JITOpt.missing_runtime ~available:[ "a"; "b" ] [ "b"; "c"; "d" ] in
  if missing <> [ "c"; "d" ]
  then fail "missing_runtime = [%s], expected [c, d]" (String.concat ", " missing);
  List.iter
    (fun name ->
       let pipeline = JITOpt.of_flags config [ name ] in
       let unmet =
         JITOpt.missing_runtime
           ~available:Engine.runtime_symbols
           (JITOpt.requires pipeline)
       in
       if unmet <> []
       then
         fail
           "pass %s needs runtime symbols missing from the engine: [%s]"
           name
           (String.concat ", " unmet))
    [ "profile"; "devirt"; "specialize"; "o2"; "tier0" ]
;;

let test_profile_pass () =
  with_tmp_file "" (fun prof_path ->
    let config : JITOpt.config = { profile_file = prof_path } in
    let prof_ir = JITOpt.run (JITOpt.of_flags config [ "profile" ]) payload in
    if String.length (IR.payload prof_ir) <= String.length (IR.payload payload)
    then fail "profile pass did not instrument the payload";
    expect_main "profile run" (run_and_close prof_ir);
    let content = read_file prof_path in
    if not (contains content "1\tmain")
    then fail "profile file %S lacks a main counter" content)
;;

let test_specialize_cold_hot () =
  with_tmp_file "1\tf\n" (fun cold_path ->
    let config : JITOpt.config = { profile_file = cold_path } in
    let cold_ir = JITOpt.run (JITOpt.of_flags config [ "specialize" ]) chain_payload in
    if drop_first_line (IR.payload cold_ir) <> IR.payload chain_payload
    then fail "cold specialize changed the payload");
  with_tmp_file "4\tf\n" (fun hot_path ->
    let config : JITOpt.config = { profile_file = hot_path } in
    let hot_ir = JITOpt.run (JITOpt.of_flags config [ "specialize" ]) chain_payload in
    if not (contains (IR.payload hot_ir) "$spec$")
    then fail "hot specialize produced no clone";
    let v = run_and_close hot_ir in
    if v <> 11L then fail "specialized main = %Ld, expected 11L" v)
;;

let test_specialize_chained () =
  with_tmp_file "4\tf\n" (fun chain_path ->
    let config : JITOpt.config = { profile_file = chain_path } in
    let chained_payload = fixture "chained.ll" in
    let chain_ir =
      try JITOpt.run (JITOpt.of_flags config [ "specialize" ]) chained_payload with
      | Failure msg -> fail "chained specialize: %s" msg
    in
    let v = run_and_close chain_ir in
    if v <> 115L then fail "chained specialized main = %Ld, expected 115L" v)
;;

let test_dump_callback () =
  with_tmp_file "4\tf\n" (fun dump_path ->
    let config : JITOpt.config = { profile_file = dump_path } in
    let seen = ref [] in
    let dump name t = seen := (name, IR.payload t) :: !seen in
    let out =
      JITOpt.run ~dump (JITOpt.of_flags config [ "profile"; "specialize" ]) chain_payload
    in
    let seen = List.rev !seen in
    let names = List.map fst seen in
    if names <> [ "profile"; "specialize" ]
    then
      fail
        "dump callback got [%s], expected [profile; specialize]"
        (String.concat "; " names);
    match seen with
    | [ (_, first); (_, second) ] ->
      if first = second then fail "two-pass dumps have identical payloads";
      if drop_first_line (IR.payload out) <> second
      then fail "last dump does not match the pipeline result"
    | _ -> fail "dump callback called %d times, expected 2" (List.length seen))
;;

let test_devirt () =
  let config : JITOpt.config = { profile_file = "unused.prof" } in
  let devirt_ir =
    JITOpt.run (JITOpt.of_flags config [ "devirt"; "o2" ]) (fixture "nonconst_chain.ll")
  in
  if contains (IR.payload devirt_ir) "call i64 @rukaml_alloc_closure"
  then fail "devirt left rukaml_alloc_closure in step";
  let v = run_and_close devirt_ir in
  if v <> 42L then fail "devirt+o2 main = %Ld, expected 42L" v
;;

let test_devirt_arities () =
  let config : JITOpt.config = { profile_file = "unused.prof" } in
  List.iter
    (fun name ->
       let out = JITOpt.run (JITOpt.of_flags config [ "devirt" ]) (fixture name) in
       let text = IR.payload out in
       if
         contains text "call i64 @rukaml_alloc_closure"
         || contains text "@rukaml_applyN(i64 %"
       then fail "%s: devirt left a closure chain" name;
       let v = run_and_close out in
       if v <> 42L then fail "%s: main = %Ld, expected 42L" name v)
    [ "chain_arity1.ll"; "chain_arity3.ll"; "chain_arity4.ll"; "chain_arity5.ll" ]
;;

let test_multi_site () =
  let config : JITOpt.config = { profile_file = "unused.prof" } in
  let out = JITOpt.run (JITOpt.of_flags config [ "devirt" ]) (fixture "multi_site.ll") in
  let text = IR.payload out in
  if not (contains text "call i64 @add(" && contains text "call i64 @mul(")
  then fail "devirt did not rewrite both sites";
  let v = run_and_close out in
  if v <> 35L then fail "multi_site main = %Ld, expected 35L" v
;;

let test_nullary () =
  let v = run_and_close (fixture "nullary.ll") in
  if v <> 42L then fail "nullary main = %Ld, expected 42L" v;
  let out =
    JITOpt.run
      (JITOpt.of_flags { profile_file = "unused.prof" } [ "devirt" ])
      (fixture "nullary.ll")
  in
  let v = run_and_close out in
  if v <> 42L then fail "nullary after devirt = %Ld, expected 42L" v
;;

let () =
  check ~name:"lifecycle" test_lifecycle;
  check ~name:"empty_pipeline" test_empty_pipeline;
  check ~name:"unknown_pass" test_unknown_pass;
  check ~name:"missing_runtime" test_missing_runtime;
  check ~name:"profile_pass" test_profile_pass;
  check ~name:"specialize_cold_hot" test_specialize_cold_hot;
  check ~name:"specialize_chained" test_specialize_chained;
  check ~name:"dump_callback" test_dump_callback;
  check ~name:"devirt" test_devirt;
  check ~name:"devirt_arities" test_devirt_arities;
  check ~name:"multi_site" test_multi_site;
  check ~name:"nullary" test_nullary;
  summary ~suite:"selfcheck" ()
;;
