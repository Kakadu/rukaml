(*
   test
  (targets (amd64 promote))
  (run (stdout "rukaml_print_int 8"))
*)
let main =
  let r = [| 1; 2; 3; 4; 5; 6; 7; 8 |] in
  let t = print (array_len r) in
  0
;;
