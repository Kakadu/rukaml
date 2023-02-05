(* https://matt.might.net/articles/a-normalization/ *)

(*     <aexp> ::= NUMBER | STRING | VAR | BOOLEAN | PRIMOP
            |  (lambda (VAR ...) <exp>)

    <cexp> ::= (<aexp> <aexp> ...)
            |  (if <aexp> <exp> <exp>)

    <exp>  ::= (let ([VAR <cexp>]) <exp>)
            |  <cexp>
            |  <aexp>

 *)
open Miniml

type a_expr =
  | AConst of int
  | AVar of string
  | APrimitive
  | ALam of string * expr

and c_expr =
  | CApp of a_expr * a_expr
  | CIte of a_expr * expr * expr

and expr =
  | ELet of Parsetree.rec_flag * string * c_expr * expr
  | EAtom of a_expr
  | EComplex of c_expr
(* 
let rec normalize : 'a. Typedtree.expr -> (expr -> 'a) -> 'a =
 fun m k ->
  match m with
  | Typedtree.TConst n -> k (EAtom (AConst n))
  | Typedtree.TLam (name, body, _) -> k (EAtom (ALam (name, normalize_term body)))
  | TLet (flg, pat, _, body, wher) ->
    normalize body (fun body -> ELet (flg, pat, EComplex body, normalize wher k))

and normalize_term t = normalize t Fun.id *)
