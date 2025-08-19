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
 (target %s.exe)
 (enabled_if
  (not
   (= none %%{read:../../../cc_rv64})))
 (mode
  (promote (until-clean)))
 (deps
  (:src %s.s)
  (:stdlib ../rukaml_stdlib.o))
 (action
  (system "%%{read:../../../cc_rv64} %%{deps} -o %%{target}")))
|}
        test
        test;
      Printf.fprintf
        ch
        {|
(cram
 (enabled_if
  (not
   (= none %%{read:../../../cc_rv64})))
 (applies_to %s)
 (deps ../rukaml_stdlib.o ./%s.exe %s.s))

|}
        test
        test
        test
    in
    List.iter on_test tests)
;;
