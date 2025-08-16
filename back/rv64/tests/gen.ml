let tests = [ "add"; "fib2"; "fac_cps" ]

let () =
  Out_channel.with_open_text "dune" (fun ch ->
    let on_test test =
      if not (Sys.file_exists (test ^ ".s"))
      then Format.eprintf "Source file for '%s' not found\n" test;
      Printf.fprintf ch ";;;;; %s\n" test;
      Printf.fprintf
        ch
        {|
(rule
 (targets %s.exe)
 (deps
  (:src %s.s)
  (:stdlib ../rukaml_stdlib.o))
 (mode
  (promote (until-clean)))
 (action
  (progn
   (run riscv64-linux-gnu-gcc-13 -g -c %%{src} -o %s.o)
   (run riscv64-linux-gnu-gcc-13 -g %%{stdlib} %s.o -o %%{targets}))))

|}
        test
        test
        test
        test;
      Printf.fprintf
        ch
        {|(cram
 (applies_to %s)
 (deps ../rukaml_stdlib.o ./%s.exe %s.s))

|}
        test
        test
        test
    in
    List.iter on_test tests)
;;
