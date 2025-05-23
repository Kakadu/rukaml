open ParseSIMD

let () =
  Callback.register "expr_printer" (fun e -> print_endline (AST.show_expr e))

let () =
  match parse_expr_string "x+1" with
  | None -> print_endline "none"
  | Some ast -> Format.printf "%a\n%!" AST.pp_expr ast
