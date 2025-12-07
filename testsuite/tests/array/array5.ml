(*
test
  (targets (amd64 promote))
  (run (stdout "rukaml_print_int 4") (stdin "adsf") )
*)

let main =
  let file = stdin in
  print (length file)
