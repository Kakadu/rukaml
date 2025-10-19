(*
test
  (src fack10.ml)
  (targets amd64)
  (flags (--cps --caa))
  (run
    (stdout
      "rukaml_print_int 2"
      "Total closure allocations: 22"))
*)
