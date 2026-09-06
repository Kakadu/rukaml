(*
test
  (targets amd64 rv32 rv64)
  (run (stdout "rukaml_print_int 2"))
*)

let inc a = a + 1

let main =
  let unit = print (inc 1) in
  0
