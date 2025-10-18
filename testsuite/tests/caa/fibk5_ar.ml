(*
test
  (src fibk5.ml)
  (targets amd64)
  (flags (-cps -call_arity))
  (run
    (stdout
      "rukaml_print_int 8"
      "Total closure allocations: 32"))
*)
