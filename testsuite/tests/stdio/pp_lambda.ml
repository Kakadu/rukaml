(*
   test
  (targets amd64 rv32 rv64)
  (run (stdout
           "i := λ x . x"
           "k := λ x . λ y . x"
           "s := λ x . λ y . λ z . (x z) (y z)"
           "omega := (λ x . x x) (λ x . x x)"
       )
  )
*)

type expr =
  | Var of char
  | Abs of (char * expr)
  | App of (expr * expr)

let pp_expr oc expr =
  let rec helper parens oc expr =
    match expr with
    | Var name -> fprintf oc "%c" name
    | Abs (name, e) ->
      fprintf oc (if parens then "(λ %c . %a)" else "λ %c . %a") name (helper false) e
    | App (e1, e2) ->
      fprintf oc (if parens then "(%a %a)" else "%a %a") (helper true) e1 (helper true) e2
  in
  helper false oc expr
;;

let main =
  let i = Abs ('x', Var 'x') in
  let k = Abs ('x', Abs ('y', Var 'x')) in
  let s =
    Abs ('x', Abs ('y', Abs ('z', App (App (Var 'x', Var 'z'), App (Var 'y', Var 'z')))))
  in
  let omega =
    App (Abs ('x', App (Var 'x', Var 'x')), Abs ('x', App (Var 'x', Var 'x')))
  in
  let () = printf "i := %a\n" pp_expr i in
  let () = printf "k := %a\n" pp_expr k in
  let () = printf "s := %a\n" pp_expr s in
  let () = printf "omega := %a\n" pp_expr omega in
  0
;;
