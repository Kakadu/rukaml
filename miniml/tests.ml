(* let rec statement () =
     let ( >>= ) = Option.bind in
     let rb1 = Rollback.make () in

     let rollback () =
       let () = Rollback.rollback rb1 in
       None
     in
     let and_semic_too rez =
       ws ();
       if char ';' then Some rez
       else
         let () = Rollback.rollback rb1 in
         None
     in
     match ident_or_keyword () with
     | Some "fi" | None -> rollback ()
     | Some "while" ->
         ws ();
         expr () >>= fun econd ->
         ws ();
         log "%s %d. pos = %d" __FILE__ __LINE__ !pos;
         if keyword "do" then (
           log "%s %d. pos = %d" __FILE__ __LINE__ !pos;
           statements () >>= fun ebody ->
           log "%s %d. pos = %d" __FILE__ __LINE__ !pos;
           ws ();
           match ident_or_keyword () with
           | None -> rollback ()
           | Some "done" ->
               ws ();
               if char ';' then Some (While (econd, ebody)) else rollback ()
           | Some _ -> rollback ())
         else rollback ()
     | Some "if" ->
         ws ();
         expr () >>= fun econd ->
         (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
         let () = ws () in
         (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
         if keyword "then" then (
           (* log "%s %d isKWthen = %b" __FILE__ __LINE__ isKWthen; *)
           (* log "%s %d. Calling 'statements'... pos = %d" __FILE__ __LINE__ !pos; *)
           statements ()
           >>= fun ethen ->
           ws ();
           (* log "%s %d. ethen.length = %d" __FILE__ __LINE__ (List.length ethen); *)
           log "%s %d. pos = %d" __FILE__ __LINE__ !pos;
           match ident_or_keyword () with
           | None ->
               (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
               let () = Rollback.rollback rb1 in
               None
           | Some "fi" -> and_semic_too (Ite (econd, ethen, []))
           | Some "else" ->
               (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
               statements () >>= fun eelse ->
               if keyword "fi" then and_semic_too (Ite (econd, ethen, eelse))
               else None
           | Some _ -> rollback ())
         else rollback ()
     | Some kw when is_keyword kw -> rollback ()
     | Some lhs -> (
         let is_eq = char '=' in
         if not is_eq then rollback ()
         else
           match expr () with
           | None -> rollback ()
           | Some rhs ->
               (* log "%s %d %s" __FILE__ __LINE__ @@ [%show: AST.expr option] rhs; *)
               if char ';' then Some (Assgn (lhs, rhs)) else rollback ())

   and statements () : _ list option =
     let rec loop acc =
       match statement () with
       | None ->
           (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
           List.rev acc
       | Some v ->
           log "Statement eaten: %s\n" (show_stmt v);
           loop (v :: acc)
     in
     let stmts = loop [] in
     Some stmts

   let parse_ident_string s =
     init s;
     ident ()

   let parse_expr_string s =
     init s;
     expr ()

   let parse_print_expr str =
     match parse_expr_string str with
     | Some ast -> Format.printf "%s\n%!" @@ [%show: AST.expr] ast
     | None -> print_endline "ERROR"

   let parse_stmt_string s =
     init s;
     statement ()

   let program () =
     let rec loop acc =
       match statement () with None -> List.rev acc | Some v -> loop (v :: acc)
     in
     let stmts = loop [] in
     ws ();
     Some stmts

   let parse_print_stmt str =
     init str;
     match statement () with
     | Some ast -> Format.printf "%s\n%!" @@ [%show: AST.stmt] ast
     | None -> print_endline "ERROR"

   let parse_print_program str =
     init str;
     match program () with
     | Some ast -> Format.printf "%s\n%!" @@ [%show: AST.stmt list] ast
     | None -> print_endline "ERROR"

   let parse ~filename =
     text := In_channel.with_open_text filename In_channel.input_all;
     init !text;
     ()
*)

let%expect_test "just ident" =
  logoff ();
  Format.printf "%s\n%!" @@ [%show: AST.expr option] (parse_expr_string "a");
  [%expect {|
  (Some (EVar "a")) |}]

let%expect_test "just a constant" =
  logoff ();
  parse_print_expr "42";
  [%expect {|
  (EConst 42) |}]

let%expect_test "a+b+c" =
  logoff ();
  parse_print_expr "a+b+c";
  [%expect
    {|
      (EBinop ("+", (EBinop ("+", (EVar "a"), (EVar "b"))), (EVar "c"))) |}]

let%expect_test " a" =
  logoff ();
  parse_print_expr " a";
  [%expect {|
            (EVar "a") |}]

let%expect_test "logic binops" =
  logoff ();
  parse_print_expr "a>0";
  [%expect {| (EBinop (">", (EVar "a"), (EConst 0))) |}];
  logoff ()

let%expect_test "a+b*c" =
  logoff ();
  parse_print_expr "a+b*c";
  [%expect
    {|
  (EBinop ("+", (EVar "a"), (EBinop ("*", (EVar "b"), (EVar "c"))))) |}]

let%expect_test "b*c+a" =
  logoff ();
  parse_print_expr "b*c+a";
  [%expect
    {|
  (EBinop ("+", (EBinop ("*", (EVar "b"), (EVar "c"))), (EVar "a"))) |}]

let%expect_test "a+b*c+1" =
  logoff ();
  parse_print_expr "a+b*c+1";
  [%expect
    {|
  (EBinop ("+",
     (EBinop ("+", (EVar "a"), (EBinop ("*", (EVar "b"), (EVar "c"))))),
     (EConst 1))) |}]

let%expect_test "various operators " =
  logoff ();
  parse_print_expr "a*b/c";
  [%expect
    {| (EBinop ("/", (EBinop ("*", (EVar "a"), (EVar "b"))), (EVar "c"))) |}];
  (* logon (); *)
  parse_print_expr "a mod b * c";
  [%expect
    {| (EBinop ("*", (EBinop ("mod", (EVar "a"), (EVar "b"))), (EVar "c"))) |}];
  logoff ()

let%expect_test "parens" =
  parse_print_expr "(b)";
  [%expect {| (EVar "b") |}];
  parse_print_expr "(b)+1";
  [%expect {| (EBinop ("+", (EVar "b"), (EConst 1))) |}];
  parse_print_expr "(b+a)*c";
  [%expect
    {| (EBinop ("*", (EBinop ("+", (EVar "b"), (EVar "a"))), (EVar "c"))) |}]

let%expect_test "program" =
  parse_print_program "x=5;";
  [%expect {|
  [(Assgn ("x", (EConst 5)))] |}]

let%test "keyword 'fi'" =
  init " fi";
  keyword "fi"

let%test "keyword 'if'" =
  init "if";
  keyword "if"

let%expect_test "stmt doesn't eat too much " =
  init " fi";
  (match statement () with
  | Some _ -> print_endline "failed"
  | None ->
      log "%s %d. pos = %d" __FILE__ __LINE__ !pos;
      Printf.printf "%b\n" (!pos < 3));
  [%expect "true"]

let%expect_test "  " =
  logoff ();
  init " fi";
  (match statement () with
  | None -> print_endline "failed"
  | Some _ -> Printf.printf "%b" (keyword "fi"));
  [%expect {|
  failed |}]

let%expect_test "  " =
  logoff ();
  init "while x do  done;";
  (match statements () with
  | Some [ _ ] -> Printf.printf "DONE"
  | _ -> print_endline "failed");
  [%expect {|
      DONE |}];
  logoff ()

let%test "keyword 'then' " =
  init " then ";
  keyword "then"

let%expect_test "program" =
  logoff ();
  parse_print_stmt "if a then fi;";
  [%expect {|
  (Ite ((EVar "a"), [], [])) |}]

let%expect_test "program 'while'" =
  logoff ();
  parse_print_stmt "while a do x=5; done;";
  [%expect {|
  (While ((EVar "a"), [(Assgn ("x", (EConst 5)))])) |}]

let%expect_test "program" =
  parse_print_stmt "x=1;";
  [%expect {|
  (Assgn ("x", (EConst 1))) |}];
  parse_print_stmt "if a then x=1; y=2; else z=3; fi;";
  [%expect
    {|
  (Ite ((EVar "a"), [(Assgn ("x", (EConst 1))); (Assgn ("y", (EConst 2)))],
     [(Assgn ("z", (EConst 3)))])) |}]

let%expect_test "program popcount" =
  parse_print_program
    {|
       x=7;
       acc=0;
       while x>0 do acc=acc+(x mod 2); x=x/2; done;
     |};
  [%expect
    {|
            [(Assgn ("x", (EConst 7))); (Assgn ("acc", (EConst 0)));
              (While ((EBinop (">", (EVar "x"), (EConst 0))),
                 [(Assgn ("acc",
                     (EBinop ("+", (EVar "acc"), (EBinop ("mod", (EVar "x"), (EConst 2)))
                        ))
                     ));
                   (Assgn ("x", (EBinop ("/", (EVar "x"), (EConst 2)))))]
                 ))
              ] |}];
  logoff ()

let factorial = {|
x=5;
acc=1;
while x>0 do acc=acc*x; x=x-1;  done;
|}

let%expect_test "program factorial" =
  parse_print_program factorial;
  [%expect
    {|
  [(Assgn ("x", (EConst 5))); (Assgn ("acc", (EConst 1)));
    (While ((EBinop (">", (EVar "x"), (EConst 0))),
       [(Assgn ("acc", (EBinop ("*", (EVar "acc"), (EVar "x")))));
         (Assgn ("x", (EBinop ("-", (EVar "x"), (EConst 1)))))]
       ))
    ] |}];
  logoff ()

let%expect_test _ =
  parse_print_stmt "while 21 do while 21 do done; done;";
  [%expect {|
  (While ((EConst 21), [(While ((EConst 21), []))]))
|}];
  logoff ()

let%expect_test _ =
  parse_print_stmt "if 21 then else if 22 then else fi; fi;";
  [%expect {| (Ite ((EConst 21), [], [(Ite ((EConst 22), [], []))])) |}];
  logoff ()
