(*
   test
  (targets amd64)
  (run (stdout
    "let rec fact = (fun n -> (if (n <= 1) then 1 else (n * (fact (n - 1)))))"
    "let rec fib = (fun n -> (if (n <= 1) then n else ((fib (n - 1)) + (fib (n - 2)))))"
       )
  )
*)

type pattern = PVar of string

type expression =
  | EInt of int
  | EVar of string
  | EIte of (expression * expression * expression)
  | ELam of (pattern * expression)
  | EApp of (expression * expression)
  | EBinop of (string * expression * expression)

type rec_flag =
  | Rec
  | NonRec

type structure_item = Pstr_value of (rec_flag * pattern * expression)

let pp_pattern oc patt =
  match patt with
  | PVar name -> fprintf oc "%s" name
;;

let rec pp_expression oc expr =
  match expr with
  | EVar name -> fprintf oc "%s" name
  | EInt n -> fprintf oc "%d" n
  | EApp (e1, e2) -> fprintf oc "(%a %a)" pp_expression e1 pp_expression e2
  | ELam (lhs, rhs) -> fprintf oc "(fun %a -> %a)" pp_pattern lhs pp_expression rhs
  | EBinop (op, lhs, rhs) ->
    fprintf oc "(%a %s %a)" pp_expression lhs op pp_expression rhs
  | EIte (e1, e2, e3) ->
    fprintf
      oc
      "(if %a then %a else %a)"
      pp_expression
      e1
      pp_expression
      e2
      pp_expression
      e3
;;

let pp_structure_item oc item =
  match item with
  | Pstr_value (Rec, lhs, rhs) ->
    fprintf oc "let rec %a = %a" pp_pattern lhs pp_expression rhs
  | Pstr_value (NonRec, lhs, rhs) -> fprintf oc "let %a = %a" pp_pattern lhs pp_expression rhs
;;

let fac_test t =
  let factorial_ast =
    Pstr_value
      ( Rec
      , PVar "fact"
      , ELam
          ( PVar "n"
          , EIte
              ( EBinop ("<=", EVar "n", EVar "1")
              , EVar "1"
              , EBinop
                  ("*", EVar "n", EApp (EVar "fact", EBinop ("-", EVar "n", EVar "1"))) )
          ) )
  in
  pp_structure_item stdout factorial_ast
;;

let fib_test t =
  let fibonacci_ast =
    Pstr_value
      ( Rec
      , PVar "fib"
      , ELam
          ( PVar "n"
          , EIte
              ( EBinop ("<=", EVar "n", EVar "1")
              , EVar "n"
              , EBinop
                  ( "+"
                  , EApp (EVar "fib", EBinop ("-", EVar "n", EVar "1"))
                  , EApp (EVar "fib", EBinop ("-", EVar "n", EVar "2")) ) ) ) )
  in
  pp_structure_item stdout fibonacci_ast
;;

let new_line t = printf "\n"

let main =
  let () = fac_test () in
  let () = new_line () in
  let () = fib_test () in
  let () = new_line () in
  0
;;
