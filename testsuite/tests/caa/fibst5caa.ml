(*
test
  (src fibst5.ml)
  (targets amd64)
  (flags (--cps --caa))
  (run
    (stdout
      "rukaml_print_int 8"
      "Total closure allocations: 24"))
*)
