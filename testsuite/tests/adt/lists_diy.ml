(*
   test
  (targets amd64)
  (run (stdout "rukaml_print_int 45"))
*)

type 'a my_list =
  | Nil
  | Cons of ('a * 'a my_list)

let rec of_list ls =
  match ls with
  | [] -> Nil
  | x :: xs -> Cons (x, of_list xs)
;;

let rec fold f acc ls =
  match ls with
  | Nil -> acc
  | Cons (x, xs) -> fold f (f acc x) xs
;;

let rec map f ls =
  match ls with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs)
;;

let rec filter p ls =
  match ls with
  | Nil -> Nil
  | Cons (x, xs) -> if p x then Cons (x, filter p xs) else filter p xs
;;

let rec merge left right =
  match left with
  | Nil -> right
  | Cons (x, xs) -> Cons (x, merge xs right)
;;

let main =
  let ls = [ [ 1; 2; 3; 4 ]; [ 5; 6; 7 ]; [ 8; 9 ]; [ 0 ]; [] ] in
  let sum = fold (fun a b -> a + b) 0 (fold merge Nil (map of_list (of_list ls))) in
  print sum
;;
