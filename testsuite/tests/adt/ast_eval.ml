(*
   test
  (targets amd64 rv64)
  (run (stdout "test passed"))
*)

type expr =
  | Int of int
  | Binop of ((int -> int -> int) * expr * expr)

let add a b = a + b
let mul a b = a * b

let rec eval tree =
  match tree with
  | Int n -> n
  | Binop (f, e1, e2) -> f (eval e1) (eval e2)
;;

let main =
  let tree =
    (* ((1 + 2) + 3) * (3 * (5 + (6 * 7))) *)
    Binop
      ( mul
      , Binop (add, Binop (add, Int 1, Int 2), Int 3)
      , Binop (mul, Int 3, Binop (add, Int 5, Binop (mul, Int 6, Int 7))) )
  in
  if eval tree = 846
    then printf "test passed"
    else
      let () = printf "test failed" in
      exit 1
