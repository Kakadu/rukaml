(*
   test
  (targets amd64)
  (run (stdout "(1, 2, 3, 4, 5)"))
*)

let main =
  match (((1, 2), 3), 4), 5 with
  | (((x1, x2), x3), x4), x5 ->
    let t = printf "(%d, %d, %d, %d, %d)" x1 x2 x3 x4 x5 in
    0
;;
