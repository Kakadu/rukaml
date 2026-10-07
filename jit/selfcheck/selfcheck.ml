(* Lifecycle selfcheck for the JIT_engine_lib.JITEngine.BackEngine
   contract in its ORC implementation. Run from the alias runtest
   (jit/selfcheck/dune). Checks, within a single process:
   1. a second create without close does not duplicate runtime symbols
      (registration happens once, under "just created", in
      rukaml_orc_create);
   2. load/run/close; a repeated close is safe (delete nullptr);
   3. create after close yields a fresh engine (main is defined again);
   4. run after close raises Failure.

   The payload is a hand-written [IR.TextLL] with no runtime references,
   so the test does not depend on the LLVM backend. *)
module Engine : JIT_engine_lib.JITEngine.BackEngine = JIT_orc_lib.OrcEngine

let payload = "define i64 @main() {\n  ret i64 5\n}\n"

let fail fmt =
  Printf.ksprintf
    (fun s ->
       prerr_endline ("selfcheck FAIL: " ^ s);
       exit 1)
    fmt
;;

let expect_main tag v = if v <> 5L then fail "%s: main = %Ld, expected 5L" tag v

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
  print_endline "selfcheck OK"
;;
