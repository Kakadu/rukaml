(*
test
  (targets rv64)
  (run (stdout "rukaml_print_int 1"))
*)

let main =
  let a = 1 in
  let x = if a < 2 then 1 else 0 in
  let y = print x in
  0
