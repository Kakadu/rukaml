(*
   test
  (targets amd64 rv64 (anf promote))
  (run (stdout "test passed"))
*)

type 'a tree =
  | Nil
  | Node of ('a * 'a tree * 'a tree)

let rec fold_infix f acc tree =
  match tree with
  | Nil -> acc
  | Node (item, left, right) -> fold_infix f (f (fold_infix f acc left) item) right
;;

type 'a option =
  | Some of 'a
  | None

let tree_max_int tree =
  match tree with
  | Nil -> None
  | Node (root_item, _, _) ->
    let max_int a b = if a < b then b else a in
    Some (fold_infix max_int root_item tree)
;;

let main =
  let tree =
    Node
      ( 1
      , Node
          ( 2
          , Node (3, Node (4, Node (5, Nil, Node (6, Nil, Nil)), Nil), Nil)
          , Node (7, Nil, Node (8, Nil, Nil)) )
      , Node (9, Node (10, Nil, Node (11, Node (12, Nil, Nil), Nil)), Nil) )
  in
  match tree_max_int tree with
  | Some 12 -> printf "test passed"
  | _ ->
    let () = printf "test failed" in
    exit 1
