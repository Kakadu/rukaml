(*
test
  (targets rv64 amd64)
  (run (stdout "rukaml_print_int 1"))
*)

let dec a = a - 1

let main =
  let unit = print (dec 2) in
  0
