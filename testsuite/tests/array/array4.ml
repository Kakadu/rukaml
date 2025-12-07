(*
test
  (targets (amd64 promote) (rv64 promote))
  (run (stdout "rukaml_print_int 5") (sh "echo test > foo"))
*)

let main =
  let file = open_in "foo" in
  print (length file)
