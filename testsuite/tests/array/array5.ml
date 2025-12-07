(*
test
  (targets (amd64 promote) )
  (run (stdout "rukaml_print_int 5") (sh "echo 'secret message' > foo"))
*)

let main =
  let file = open_in "foo" in
  print (length file)
