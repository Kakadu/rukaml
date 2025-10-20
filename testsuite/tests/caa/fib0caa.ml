(*
test
  (src fib0.ml)
  (targets amd64)
  (flags (--cps --caa))
  (run
    (stdout
      "rukaml_print_int 8"
      "Total closure allocations: 32"))
*)
