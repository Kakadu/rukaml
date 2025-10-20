(*
test
  (targets rv64 amd64)
  (run (stdout "rukaml_print_int 101"))
*)

let sum a b k = k (a + b)

let main =
  let w = sum 100 1 print in
  0
