(*
   test
  (targets amd64)
  (run (stdout "4"))
*)

let tuple = ((1, 2, 3), (4, (5, 6)), 7), 8

let first_of_tuple2 t =
  match t with
  | x, y -> x
;;

let second_of_tuple3 t =
  match t with
  | x, y, z -> y
;;

let four = first_of_tuple2 (second_of_tuple3 (first_of_tuple2 tuple))
let main = printf "%d" four
