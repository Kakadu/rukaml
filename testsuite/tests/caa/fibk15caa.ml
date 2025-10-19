(*
test
  (src fibk15.ml)
  (targets amd64)
  (flags (--cps --caa))
  (run
    (stdout
      "rukaml_print_int 987"
      "Total closure allocations: 3948"))
*)
