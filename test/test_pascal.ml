let () = print_endline "hello"

let () =
  let open AST in
  let prog = [], [ Assgn ("x", EVar "x") ] in
  print_endline @@ show_program prog
;;
