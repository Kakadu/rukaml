(*
test
  (targets (amd64 promote) (rv64 promote))
  (run (stdout "rukaml_print_int 42"))
*)
let main =
  let r = [|1; 4; 3; 3|] in
  let unit = array_set r 1 42 in
  print (array_get r 1)
