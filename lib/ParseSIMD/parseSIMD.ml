external p_expr_stub : string -> (AST.expr * int) option = "p_expr_stub"

let parse_expr_string str =
  match p_expr_stub str with Some (ast, _) -> Some ast | None -> None

let%expect_test _ =
  (match parse_expr_string "a+1" with
  | Some ast -> Format.printf "%a" AST.pp_expr ast
  | None -> print_endline "none");
  [%expect "
    (EVar \"a\")"]
