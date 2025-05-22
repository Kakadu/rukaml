external p_expr_stub : string -> (AST.expr * int) option = "p_expr_stub"

let parse_expr_string str =
  match p_expr_stub str with Some (ast, _) -> Some ast | None -> None
