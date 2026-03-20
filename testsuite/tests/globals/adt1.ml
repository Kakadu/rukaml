(*
   test
  (targets amd64)
  (run (stdout "15"))
*)

let list = [ 1; 2; 3; 4; 5 ]

let rec sum ls =
  match ls with
  | [] -> 0
  | hd :: tl -> hd + sum tl
;;

let main = printf "%d" (sum list)
