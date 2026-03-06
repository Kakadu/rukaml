(*
test
  (targets amd64)
  (run (stdout "rukaml_print_int 1"))
*)

let main =
  let pp_sum oc a b = fprintf oc "(%d + %d)" a b in
  let u = pp_sum stdout 1 2 in
  0
