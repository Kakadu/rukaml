let preamble = {|(cram
 (applies_to *)
 (deps
  (package MiniML)
  (package MiniML_rv64)
  ; %{project_root}/compiler.exe
  %{project_root}/back_rv64/RV64_compiler.exe
  %{project_root}/back_rv64/rukaml_stdlib.o
  ;
  ))

|}

let tests =
  [ "empty";
    "fac";
    "ite0";
    "fib";
    "fib_acc";
    "add";
    "fac_acc";
    "fac_cps";
    "long_app";
    "pass_print";
    "pass_print2";
    "anon1";
    "order1";
    "nikita2";
    "nikita3";
    "fib_cps";
  ]

let () =
  Out_channel.with_open_text "dune" (fun ch ->
    Printf.fprintf ch "%s" preamble;
    let on_test test =
      if not (Sys.file_exists (test^".ml"))
        then Format.eprintf "Source file for '%s' not found\n" test;

      Printf.fprintf ch ";;;;; %s\n" test;
      Printf.fprintf ch {|
(rule
 (targets %s.s)
 (deps
  %%{project_root}/back_rv64/RV64_compiler.exe
  (:src %s.ml))
 (mode
  (promote (until-clean)))
 (action
  (progn
   (run
    %%{project_root}/back_rv64/RV64_compiler.exe
    -o
    %%{targets}
    --no-start
    %%{src}))))

|} test test;

      Printf.fprintf ch {|
(rule
 (targets %s.exe)
 (deps
  %%{project_root}/back_rv64/RV64_compiler.exe
  (:src %s.s)
  %%{project_root}/back_rv64/rukaml_stdlib.o)
 (mode
  (promote (until-clean)))
 (action
  (progn
   (run riscv64-linux-gnu-gcc-13 -g -c %%{src} -o %s.o)
   (run
    riscv64-linux-gnu-gcc-13
    -g
    %%{project_root}/back_rv64/rukaml_stdlib.o
    %s.o
    -o
    %%{targets}))))

|} test test test test;

    Printf.fprintf ch {|(cram
 (applies_to %s)
 (deps
  %%{project_root}/back_rv64/rukaml_stdlib.o
  ./%s.exe
  %s.ml
  %s.s))

|}
      test test test test
    in
    List.iter on_test tests
    )