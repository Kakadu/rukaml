(*
test
  (targets rv64 amd64)
  (run (stdout "rukaml_print_int 11"))
*)

let add a b = a + b

let main =
  let x = add 1 10 in
  let y = print x in
  0
