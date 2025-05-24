open Parser

let () =
  match parse_expr_string "x*(3)" with
  | None -> print_endline "none"
  | Some ast -> Format.printf "%a\n%!" AST.pp_expr ast
