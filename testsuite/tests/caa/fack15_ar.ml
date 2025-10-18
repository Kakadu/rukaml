(*
test
  (src fack15.ml)
  (targets amd64)
  (flags (-cps -call_arity))
  (run
    (stdout
      "rukaml_print_int 2"
      "Total closure allocations: 32"))
*)
