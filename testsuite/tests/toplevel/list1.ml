(*
   test
  (targets amd64)
  (run (stdout "test passed"))
*)

let list = [ 1; 2; 3; 4; 5 ]

let rec sum ls =
  match ls with
  | [] -> 0
  | hd :: tl -> hd + sum tl
;;

let main = printf "%s" (if sum list = 15 then "test passed" else "test failed")
