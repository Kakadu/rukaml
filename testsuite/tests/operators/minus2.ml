(*
test
  (targets amd64 rv32 rv64)
  (run (stdout "rukaml_print_int -3"))
*)

let f a = a - 5

let main =
  let unit = print (f 2) in
  0
