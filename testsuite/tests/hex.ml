(*
test
  (targets   rv64 amd64)
  (run (stdout "rukaml_print_int 11" "rukaml_print_int 255"))
*)

let add a b = a + b

let main =
  let x = add 1 0xa in
  let y = print x in
  let () = print 0xFF in
  0
