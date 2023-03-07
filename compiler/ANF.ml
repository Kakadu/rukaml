(* https://matt.might.net/articles/a-normalization/ *)

open Miniml

(* type a_expr =
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

let rec normalize : 'a. Typedtree.expr -> (expr -> 'a) -> 'a =
 fun m k ->
  match m with
  | Typedtree.TConst n -> k (EAtom (AConst n))
  | Typedtree.TLam (name, body, _) -> k (EAtom (ALam (name, normalize_term body)))
  | TLet (flg, pat, _, body, wher) ->
    normalize body (fun body -> ELet (flg, pat, EComplex body, normalize wher k))

and normalize_c : 'a. Typedtree.expr -> (expr -> 'a) -> 'a =


and normalize_term t = normalize t Fun.id
 *)
(*     <aexp> ::= NUMBER | STRING | VAR | BOOLEAN | PRIMOP
            |  (lambda (VAR ...) <exp>)

    <cexp> ::= (<aexp> <aexp> ...)
            |  (if <aexp> <exp> <exp>)

    <exp>  ::= (let ([VAR <cexp>]) <exp>)
            |  <cexp>
            |  <aexp>

 *)
type _ expr =
  | AConst : int -> [ `Atom ] expr
  | AVar : string -> [ `Atom ] expr
  | APrimitive : [ `Atom ] expr
  | ALam : string * _ expr -> [ `Atom ] expr
  (*  *)
  | CApp : [ `Atom ] expr * [ `Atom ] expr -> [ `Complex ] expr
  | CIte : [ `Atom ] expr * [ `Default ] expr * [ `Default ] expr -> [ `Complex ] expr
  | ELet : Parsetree.rec_flag * string * [ `Complex ] expr * _ expr -> [ `Default ] expr
  (*  *)
  | EAtom : [ `Atom ] expr -> [ `Default ] expr
  | EComplex : [ `Complex ] expr -> [ `Default ] expr

let pp : 'a. Format.formatter -> 'a expr -> unit = fun ppf e -> Format.fprintf ppf "<anf>"

let rec normalize : 'a 'b. Typedtree.expr -> ([ `Default ] expr -> 'a) -> 'a =
 fun m k ->
  match m with
  | Typedtree.TConst (PConst_int n) -> k (EAtom (AConst n))
  | TLam (name, body, _) -> k (EAtom (ALam (name, normalize_term body)))
  | TLet (flg, pat, _, body, wher) ->
    normalize_c body (fun body ->
      normalize wher (fun wher -> k (ELet (flg, pat, body, wher))))
  (* Chaining below *)
  | TIf _ | TApp _ -> normalize_c m (fun c -> k (EComplex c))
  | _ -> Format.kasprintf failwith "Not implemented; '%a'" Pprinttyped.pp_hum m

and normalize_c : 'a 'b. Typedtree.expr -> ([ `Complex ] expr -> 'a) -> 'a =
 fun m k ->
  match m with
  (*   | Typedtree.TApp (l, r, _) ->
    normalize_a l (fun l -> normalize_a r (fun r -> k (CApp (l, r)))) *)
  | TIf (cond, th, el, _ty) ->
    normalize_a cond (fun cond ->
      normalize th (fun th -> normalize el (fun el -> k (CIte (cond, th, el)))))
  | TApp (l, r, _) ->
    normalize_name l (fun l -> normalize_name r (fun r -> k (CApp (l, r))))

and normalize_a : 'a 'b. Typedtree.expr -> ([ `Atom ] expr -> 'a) -> 'a =
 fun m k ->
  match m with
  | Typedtree.TConst (PConst_int n) -> k (AConst n)
  | _ ->
    Format.kasprintf failwith "Not implemented in 'normalize_a': %a" Pprinttyped.pp_hum m

and normalize_name : 'a 'b. Typedtree.expr -> ([ `Atom ] expr -> 'a) -> 'a =
 fun m k ->
  match m with
  | Typedtree.TConst (PConst_int n) -> k (AConst n)

and normalize_term t = normalize t Fun.id

let%expect_test " " =
  let text = {| let rec fac = fun n -> if n=0 then 1 else n * (fac (n-1))|} in
  let ( let* ) x f = Result.bind x f in
  (let stru = Miniml.Parsing.parse_vb_exn text in
   let* { tvb_body = typed; _ } = Inferencer.vb stru in
   let anf = normalize_term typed in
   Format.printf "%a\n%!" pp anf;
   Result.Ok ())
  |> ignore;
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Not implemented in 'normalize_a': (n = 0)")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Compile_lib__ANF.normalize in file "compiler/ANF.ml", line 63, characters 50-69
  Called from Compile_lib__ANF.(fun) in file "compiler/ANF.ml", line 101, characters 13-33
  Called from Stdlib__Result.bind in file "result.ml" (inlined), line 23, characters 36-39
  Called from Compile_lib__ANF.(fun).let* in file "compiler/ANF.ml", line 98, characters 21-36
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
;;
