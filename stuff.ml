open Parsetree
open Angstrom

let parse p str = parse_string ~consume:All (p Parsing.pack) str

let ok ~parser ~pp str ast =
  match parse_string ~consume:All (parser Parsing.pack) str with
  | Result.Error s ->
    Format.eprintf "parsing failed: %s\n%!" s;
    false
  | Result.Ok e when e = ast -> true
  | Ok e ->
    Format.printf "Got: %a\n" pp e;
    false
;;

let okexpr = ok ~parser:Parsing.(pack.expr_long) ~pp:pp_expr
let okident = ok ~parser:(fun _ -> Parsing.ident) ~pp:(fun ppf -> Format.fprintf ppf "%s")
let oknumber = ok ~parser:(fun _ -> Parsing.number) ~pp:Format.pp_print_int

(* let oksum = ok ~parser:Parsing.(pack.sum) ~pp:pp_expr *)
(* let okprod = ok ~parser:Parsing.(pack.product) ~pp:pp_expr *)
let okprio = ok ~parser:Parsing.(pack.prio) ~pp:pp_expr

(* let okstru = ok ~parser:Parsing.(fun _ -> structure) ~pp:pp_structure *)
(* let oktyp = ok ~parser:Parsing.(fun _ -> typ) ~pp:pp_typ *)

(* let%test _ = okexpr "1" (econst 1) *)
(* let%test _ = okexpr "(1)" (econst 1) *)
(* let%test _ = oknumber "1" 1 *)
let%test _ = okident "a" "a"
let%test _ = okexpr "(1)" (econst 1)
(* let%test _ = okexpr "(fun _1 -> _1)" (elam (pvar 1) (evar 1)) *)

let%expect_test _ =
  let ast = Base.Result.ok_or_failwith @@ parse Parsing.(pack.expr_long) "f (x x)" in
  print_string @@ Parsetree.show_expr ast;
  [%expect {| EApp (EVar ("f"), EApp (EVar ("x"), EVar ("x"))) |}]
;;

let%test _ =
  okexpr
    "(let x = y in fun y -> x)"
    (elet (pvar "x") (evar "y") (elam (pvar "y") (evar "x")))
;;
(*
let%test _ = okexpr "_1 _2" (eapp (evar 1) [ evar 2 ])

(* let%test _ =
  okexpr "let rec _1 = 1 in 1" (elet ~isrec:true (pvar 1) (econst 1) (econst 1))
;;
 *)
let%test _ =
  okexpr
    "let rec _1 = fun _2 -> _1 _2 in _1"
    (elet ~isrec:true (pvar 1) (elam (pvar 2) (eapp (evar 1) [ evar 2 ])) (evar 1))
;;

let%test _ =
  okexpr
    "let rec _1 _2 = _1 _2 in _1"
    (elet ~isrec:true (pvar 1) (elam (pvar 2) (eapp (evar 1) [ evar 2 ])) (evar 1))
;;

(* let%test _ = oksum "1+2" (eadd (econst 1) (econst 2)) *)
(* let%test _ = okprod "1*2" (emul (econst 1) (econst 2)) *)
(* let%test _ = oksum "1+2*3" (eadd (econst 1) (emul (econst 2) (econst 3))) *)
(* let%test _ = okprio "1+2*3" (eadd (econst 1) (emul (econst 2) (econst 3))) *)
(* let%test _ = okexpr "if 1 then 2 else 3" (eite (econst 1) (econst 2) (econst 3)) *)
(* let%test _ = okprio "1+2*3" (eadd (econst 1) (emul (econst 2) (econst 3))) *)
(* let%test _ = okprio "1+2" (eadd (econst 1) (econst 2)) *)

(* let%test _ =
  okprio "1*2+3*4" (eadd (emul (econst 1) (econst 2)) (emul (econst 3) (econst 4)))
;;

let%test _ =
  okprio "1*2+3*4" (eadd (emul (econst 1) (econst 2)) (emul (econst 3) (econst 4)))
;;

let%test _ =
  okprio "f x + f y" (eadd (eapp (evar "f") [ evar "x" ]) (eapp (evar "f") [ evar "y" ]))
;;

let%test _ = okprio "f x + 1" (eadd (eapp (evar "f") [ evar "x" ]) (econst 1))
let%test _ = okprio "z+f x" (eadd (evar "z") (eapp (evar "f") [ evar "x" ]))
 *)

(*
let%test _ =
  okprio "if n=1 then 2 else 3" (eite (eeq (evar "n") (econst 1)) (econst 2) (econst 3))
;;

let%test _ =
  okprio
    "let rec fac n = if n=1 then 1 else n*(fac (n-1)) in fac 5"
    (elet
       ~isrec:true
       (pvar "fac")
       (elam
          (pvar "n")
          (eite
             (eeq (evar "n") (econst 1))
             (econst 1)
             (emul (evar "n") (eapp (evar "fac") [ esub (evar "n") (econst 1) ]))))
       (eapp (evar "fac") [ econst 5 ]))
;;
*)

(* let%test _ = okprio "2-1" (esub (econst 2) (econst 1)) *)

(* let%test _ = okprio "(fun _1 -> _2): int" (econstraint (elam (pvar 1) (evar 2)) tint) *)

(* let%test _ =
  okprio
    "(fun _1 -> _2): int -> int"
    (econstraint (elam (pvar 1) (evar 2)) (tarrow tint tint))
;;
 *)
let%test _ = oktyp "int->int" (tarrow tint tint)
(* let%test _ = okprio "(fun (_1:int) -> _1)" (elam (pconstraint (pvar 1) tint) (evar 1)) *)

let%test _ =
  okstru
    "let rec _1 _2 = _1 _2\nlet _3 = _5"
    [ slet Recursive (pvar 1) (elam (pvar 2) (eapp (evar 1) [ evar 2 ]))
    ; slet Nonrecursive (pvar 3) (evar 5)
    ]
;;

let%expect_test _ =
  let ast = Base.Result.ok_or_failwith @@ parse Parsing.(pack.expr) "(fun _1 -> _1)" in
  print_string @@ Parsetree.show_expr ast;
  [%expect {| ELam (PVar (1), EVar (1)) |}]
;;

let%expect_test _ =
  let ast =
    Base.Result.ok_or_failwith
    @@ parse
         Parsing.(pack.expr_long)
         "fun _f -> (fun _x -> _f (_x _x)) (fun _x -> _f (_x _x))"
  in
  print_string @@ Parsetree.show_expr ast;
  [%expect
    {|
    EApp
    (ELam
     (PVar (54), ELam (PVar (72), EApp (EVar (54), EApp (EVar (72), EVar (72))))),
     ELam (PVar (72), EApp (EVar (54), EApp (EVar (72), EVar (72))))) |}];
  (try print_endline @@ Typedtree.show_ty @@ Inferencer.w ast with
  | Inferencer.Occurs_check -> ());
  [%expect {| |}]
;;

let%expect_test _ =
  let ast =
    Base.Result.ok_or_failwith
    @@ parse Parsing.(pack.expr_long) "fun _f -> fun _x -> _f _x"
  in
  print_string @@ Parsetree.show_expr ast;
  [%expect {|
    ELam (PVar (54), ELam (PVar (72), EApp (EVar (54), EVar (72)))) |}];
  (try Format.printf "%a\n%!" Pprint.pp_typ (Inferencer.w ast) with
  | Inferencer.Occurs_check -> ());
  [%expect {|
    (('_73 -> '_74) -> ('_73 -> '_74)) |}]
;;
(*
let%expect_test _ =
  let ast = Base.Result.ok_or_failwith @@ parse Parsing.(pack.expr_long) "5" in
  print_string @@ Parsetree.show_expr ast;
  [%expect {|
    EConst (5) |}];
  (try Format.printf "%a\n%!" Pprint.pp_typ (Inferencer.w ast) with
  | Inferencer.Occurs_check -> ());
  [%expect {| forall  . int |}]
;;

let%expect_test _ =
  let ast =
    Base.Result.ok_or_failwith
    @@ parse Parsing.(pack.expr_long) "let _i = fun _x -> _x in _i 5"
  in
  print_string @@ Parsetree.show_expr ast;
  [%expect
    {|
    ELet
    (false, PVar (57), ELam (PVar (72), EVar (72)), EApp (EVar (57), EConst (5))) |}];
  (try Format.printf "%a\n%!" Pprint.pp_typ (Inferencer.w ast) with
  | Inferencer.Occurs_check -> ());
  [%expect {|
    forall  . int |}]
;;

let%expect_test _ =
  let open Inferencer in
  let subst = unify (Arrow (V 1, V 1)) (Arrow (Prim "int", V 2)) in
  let open Caml.Format in
  printf
    "[ %a ]"
    (pp_print_list
       ~pp_sep:(fun ppf () -> fprintf ppf " ")
       (fun ppf (k, v) -> fprintf ppf "%d -> %a" k Pprint.pp_typ v))
    subst;
  [%expect {| [ 2 -> int 1 -> int ] |}]
;;
*)
 *)
