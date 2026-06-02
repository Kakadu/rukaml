(*
   test
  (targets amd64)
  (run (stdout
    "infix: (((1 + 2) + 3) * (3 * (5 + (6 * 7))))"
    "rpn: 12+3+3567*+**"))
*)

type expr =
  | Int of int
  | Binop of (string * expr * expr)

let rec pp_expr_rpn oc expr =
  match expr with
  | Int n -> fprintf oc "%d" n
  | Binop (op, e1, e2) -> fprintf oc "%a%a%s" pp_expr_rpn e1 pp_expr_rpn e2 op
;;

let rec pp_expr_infix oc expr =
  match expr with
  | Int n -> fprintf oc "%d" n
  | Binop (op, e1, e2) -> fprintf oc "(%a %s %a)" pp_expr_infix e1 op pp_expr_infix e2
;;

let main =
  let expr =
    (* ((1 + 2) + 3) * (3 * (5 + (6 * 7))) *)
    Binop
      ( "*"
      , Binop ("+", Binop ("+", Int 1, Int 2), Int 3)
      , Binop ("*", Int 3, Binop ("+", Int 5, Binop ("*", Int 6, Int 7))) )
  in
  let t = printf "infix: %a\n" pp_expr_infix expr in
  let t = printf "rpn: %a\n" pp_expr_rpn expr in
  0
;;
