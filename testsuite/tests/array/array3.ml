(*
   test
  (targets (amd64 promote))
  (run (stdout "rukaml_print_int 48"))
*)

let main =
  let r = [| '0'; '1'; '2'; '3' |] in
  let a = array_get r 0 in
  let t = print (char_code a) in
  0
;;
