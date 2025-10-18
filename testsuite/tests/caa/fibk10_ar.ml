(*
test
  (src fibk10.ml)
  (targets amd64)
  (flags (-cps -call_arity))
  (run
    (stdout
      "rukaml_print_int 89"
      "Total closure allocations: 356"))
*)
