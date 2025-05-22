open Parser

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
