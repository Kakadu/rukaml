(*
   test
  (targets rv32 amd64)
  (run (stdout "(1, 2, 3)"))
*)

let tuple = 1, 2, 3

let main =
  match tuple with
  | x1, x2, x3 -> printf "(%d, %d, %d)" x1 x2 x3
;;
