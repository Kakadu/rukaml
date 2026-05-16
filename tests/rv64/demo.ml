(*
   test
  (targets amd64 rv64)
  (run (stdout
          "((fun x -> x) (fun x -> x))"
          "(let x = 1 in (let y = 2 in ((+ x) y)))"
          "(let a = 10 in (let b = 20 in (let c = ((+ a) b) in ((== c) 30))))"
          "(let a = 1 in (let b = 2 in (let c = 3 in (let d = 4 in (let e = 5 in (((((+ a) b) c) d) e))))))"
          "(fun _ -> (fun _ -> (fun _ -> (fun _ -> (fun _ -> ())))))"
       )
  )
*)

type constant =
  | CUnit
  | CInt of int
  | CStr of string
  | CBool of bool

type pattern =
  | PAny
  | PVar of string
  | PConstant of constant

type expression =
  | EVar of string
  | EConstant of constant
  | EApp of (expression * expression)
  | ELam of (pattern * expression)
  | ELet of (pattern * expression * expression)

let pp_constant oc const =
  match const with
  | CUnit -> fprintf oc "()"
  | CInt n -> fprintf oc "%d" n
  | CStr s -> fprintf oc "%s" s
  | CBool b -> fprintf oc "%b" b
;;

let pp_pattern oc patt =
  match patt with
  | PAny -> fprintf oc "_"
  | PVar name -> fprintf oc "%s" name
  | PConstant const -> pp_constant oc const
;;

let rec pp_expression oc expr =
  match expr with
  | EVar name -> fprintf oc "%s" name
  | EConstant const -> pp_constant oc const
  | EApp (e1, e2) -> fprintf oc "(%a %a)" pp_expression e1 pp_expression e2
  | ELam (lhs, rhs) -> fprintf oc "(fun %a -> %a)" pp_pattern lhs pp_expression rhs
  | ELet (lhs, rhs, body) ->
    fprintf oc "(let %a = %a in %a)" pp_pattern lhs pp_expression rhs pp_expression body
;;

let test1 t =
  let expr =
    let id = ELam (PVar "x", EVar "x") in
    EApp (id, id)
  in
  pp_expression stdout expr
;;

let test2 t =
  let expr =
    ELet
      ( PVar "x"
      , EConstant (CInt 1)
      , ELet (PVar "y", EConstant (CInt 2), EApp (EApp (EVar "+", EVar "x"), EVar "y")) )
  in
  pp_expression stdout expr
;;

let test3 t =
  let expr =
    ELet
      ( PVar "a"
      , EConstant (CInt 10)
      , ELet
          ( PVar "b"
          , EConstant (CInt 20)
          , ELet
              ( PVar "c"
              , EApp (EApp (EVar "+", EVar "a"), EVar "b")
              , EApp (EApp (EVar "==", EVar "c"), EConstant (CInt 30)) ) ) )
  in
  pp_expression stdout expr
;;

let test4 t =
  let expr =
    ELet
      ( PVar "a"
      , EConstant (CInt 1)
      , ELet
          ( PVar "b"
          , EConstant (CInt 2)
          , ELet
              ( PVar "c"
              , EConstant (CInt 3)
              , ELet
                  ( PVar "d"
                  , EConstant (CInt 4)
                  , ELet
                      ( PVar "e"
                      , EConstant (CInt 5)
                      , EApp
                          ( EApp
                              ( EApp (EApp (EApp (EVar "+", EVar "a"), EVar "b"), EVar "c")
                              , EVar "d" )
                          , EVar "e" ) ) ) ) ) )
  in
  pp_expression stdout expr
;;

let test5 t =
  let expr =
    ELam (PAny, ELam (PAny, ELam (PAny, ELam (PAny, ELam (PAny, EConstant CUnit)))))
  in
  pp_expression stdout expr
;;

let new_line t = printf "\n"

let main =
  let t = test1 () in
  let t = new_line () in
  let t = test2 () in
  let t = new_line () in
  let t = test3 () in
  let t = new_line () in
  let t = test4 () in
  let t = new_line () in
  let t = test5 () in
  0
;;
