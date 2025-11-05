(*
test
  (targets (rv64 promote) (amd64 promote))
  (run (stdout "rukaml_print_int 3"))

*)
let main =
  let r = [|1; 2; 3|] in
  let n = length r in
  print n
