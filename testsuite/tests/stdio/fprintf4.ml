(*
test
  (targets amd64)
  (run (stdout "rukaml_print_int 1"))
*)

let main =
  let pp_int oc n = fprintf oc "%d" n in
  let u = printf "%a" pp_int 42 in
  0
