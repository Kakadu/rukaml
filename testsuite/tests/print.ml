(*
test
  (targets amd64)
  (run (stdout "rukaml_print_int 20"))
*)

let prod a b c = 2 * c

let main =
  let w = prod 1 8 10 in
  let u = print w in
  0
