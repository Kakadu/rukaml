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
type atom = [ `Atom ]

type _ expr =
  | AUnit : [ `Atom ] expr
  | AConst : Parsetree.const -> [ `Atom ] expr
  | AVar : string -> [ `Atom ] expr
  | APrimitive : [ `Atom ] expr
  | ALam : string * _ expr -> [ `Atom ] expr
  (*  *)
  | CApp : [ `Atom ] expr * [ `Atom ] expr -> [ `Complex | `Atom ] expr
  | CIte :
      [ `Atom ] expr
      * [ `Atom | `Complex | `Default ] expr
      * [ `Atom | `Complex | `Default ] expr
      -> [ `Complex ] expr
  | CAtom : [ `Atom ] expr -> [ `Complex ] expr
  (*  *)
  | ELet :
      Parsetree.rec_flag
      * string
      * [ `Complex | `Atom ] expr
      * [ `Atom | `Complex | `Default ] expr
      -> [ `Atom | `Complex | `Default ] expr
  | EComplex : [ `Complex ] expr -> [ `Default ] expr

type any_expr = [ `Atom | `Complex | `Default ] expr

let rec pp : 'a. Format.formatter -> 'a expr -> unit =
  let open Format in
  fun (type a) ppf : (a expr -> unit) -> function
    | AUnit -> fprintf ppf "()"
    | APrimitive -> fprintf ppf "<prim>"
    | AConst (Parsetree.PConst_int n) -> pp_print_int ppf n
    | AConst (Parsetree.PConst_bool n) -> pp_print_bool ppf n
    | AVar name -> fprintf ppf "%s" name
    | ALam (name, rhs) -> fprintf ppf "(fun %s -> %a)" name pp rhs
    | CApp (l, r) -> fprintf ppf "(%a %a)" pp l pp r
    | CAtom ea -> pp ppf ea
    | CIte (cond, th, el) -> fprintf ppf "(if %a then %a else %a)" pp cond pp th pp el
    | ELet (flg, name, rhs, wher) ->
      fprintf
        ppf
        "@[<v>@[let %s%s = %a in@]@,@[%a@]@]"
        (match flg with
         | Recursive -> "rec "
         | _ -> "")
        name
        pp
        rhs
        pp
        wher
    | EComplex ec -> pp ppf ec
;;

let gensym =
  let n = ref 0 in
  fun () ->
    incr n;
    !n
;;

open Monads.Cont
open Monads.Cont.Syntax

let to_any : _ expr -> any_expr = Obj.magic
let complex_to_atom_comlex : [ `Complex ] expr -> [ `Complex | `Atom ] expr = Obj.magic
let complex_to_any : [ `Complex | `Atom ] expr -> any_expr = Obj.magic

let rec normalize : Typedtree.expr -> any_expr Monads.Cont.t =
 fun m ->
  match m with
  | Typedtree.TConst n -> return (EComplex (CAtom (AConst n)) |> to_any)
  | TLam (name, body, _) ->
    return (EComplex (CAtom (ALam (name, normalize_term body))) |> to_any)
  | TLet (flg, pat, typ, body, wher) ->
    (match body with
     | TLet (flg2, pat2, typ2, body2, wher) ->
       normalize (TLet (flg2, pat2, typ2, body2, TLet (flg, pat, typ, body, wher)))
     | Typedtree.TLam (pat, expr, _) ->
       let* anf_expr = normalize expr in
       let* anf_wher = normalize wher in
       return
         (ELet
            (flg, pat, CAtom (ALam ("arg", anf_expr)) |> complex_to_atom_comlex, anf_wher)
          |> to_any)
     | _ -> assert false)
  | TIf (TLet (flg, name, rhs_typ, rhs, wher), eth, el, iftyp) ->
    normalize (TLet (flg, name, rhs_typ, rhs, TIf (wher, eth, el, iftyp)))
  | TIf (econd, eth, el, _) ->
    let* anf_cond = normalize econd in
    let rec hack (type a) : a expr -> _ = function
      | ALam _ -> failwith "unreachable"
      | AConst _ as anf_cond ->
        let* anf_th = normalize eth in
        let* anf_el = normalize el in
        return (CIte (anf_cond, anf_th, anf_el) |> to_any)
      | ELet (flg, name, rhs, wher) ->
        let* inner = hack wher in
        return (ELet (flg, name, rhs, inner))
      | CApp (f, arg) ->
        let* anf_th = normalize eth in
        let* anf_el = normalize el in
        let tempname = gensym () |> Printf.sprintf "cond%d" in
        return
          (ELet
             ( NonRecursive
             , tempname
             , CApp (f, arg)
             , EComplex (CIte (AVar tempname, anf_th, anf_el)) |> to_any ))
      | anf ->
        Format.kasprintf failwith "Not implemented; '%a' %s %d" pp anf __FILE__ __LINE__
    in
    hack anf_cond
  | TApp (f, arg, _) ->
    let rec wrap (type a) : ([ `Atom ] expr -> any_expr) -> a expr -> any_expr =
     fun k -> function
      | (AUnit as v)
      | (AConst _ as v)
      | (AVar _ as v)
      | CAtom (AConst _ as v)
      | EComplex (CAtom (AConst _ as v)) -> k v
      | ALam _ as v -> k v
      | EComplex (CAtom (ALam _ as v)) -> k v
      | CAtom (ALam _ as v) -> k v
      | CApp (l, r) ->
        let tempname = gensym () |> Printf.sprintf "name%d" in
        ELet (NonRecursive, tempname, CApp (l, r), k (AVar tempname)) |> to_any
      | ELet (flg, name, rhs, wher) -> ELet (flg, name, rhs, wrap k wher) |> to_any
      | anf ->
        Format.kasprintf failwith "Not implemented; '%a' %s %d" pp anf __FILE__ __LINE__
    in
    let* anf_f = normalize f in
    let* anf_arg = normalize arg in
    wrap
      (fun atomf ->
        wrap (fun atom_arg -> CApp (atomf, atom_arg) |> complex_to_any) anf_arg)
      anf_f
    |> return
  | TVar ("=", _) -> return (AVar "=" |> to_any)
  | TVar (name, _) -> return (AVar name |> to_any)
  | _ -> Format.kasprintf failwith "Not implemented; '%a'" Pprinttyped.pp_hum m

(*
and normalize_a : 'a 'b. Typedtree.expr -> ([ `Atom ] expr -> 'a) -> 'a =
 fun m k ->
  match m with
  | Typedtree.TConst n -> k (AConst n)
  | TUnit -> k AUnit
  | TVar (name, _) -> k (AVar name)
  | _ ->
    Format.kasprintf failwith "Not implemented in 'normalize_a': %a" Pprinttyped.pp_hum m
 *)
(* and normalize_name : 'a 'b. Typedtree.expr -> ([ `Atom ] expr -> 'a) -> 'a =
 fun m k ->
  match m with
  | Typedtree.TConst n -> k (AConst n)
 *)
and normalize_term t = Monads.Cont.run_cont Fun.id (normalize t)

let test_anf text =
  let ( let* ) x f = Result.bind x f in
  match
    let stru = Miniml.Parsing.parse_vb_exn text in
    (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
    let* { tvb_body = typed; _ } = Inferencer.vb stru in
    (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
    Result.ok (normalize_term typed)
  with
  | Result.Error err -> Format.printf "%a\n%!" Inferencer.pp_error err
  | Ok anf -> Format.printf "%a\n%!" pp anf
;;

let%expect_test "standard factorial" =
  test_anf {| let rec fac = fun n -> if n=0 then 1 else n * (fac (n-1))|};
  [%expect
    {|
    (fun n -> let name1 = (= n) in
              let cond6 = (name1 0) in
              (if cond6 then 1 else let name4 = (* n) in
                                    let name2 = (- n) in
                                    let name3 = (name2 1) in
                                    let name5 = (fac name3) in
                                    (name4 name5))) |}]
;;

let%expect_test "exponential fibonacci" =
  test_anf
    {| let rec fib = fun n ->
        if n=0 then 0 else
        if n=1 then 1 else
        fib (n-1) + fib (n-2)|};
  [%expect
    {|
    (fun n -> let name7 = (= n) in
              let cond17 = (name7 0) in
              (if cond17 then 0 else let name8 = (= n) in
                                     let cond16 = (name8 1) in
                                     (if cond16 then 1 else let name9 = (- n) in
                                                            let name10 = (name9 1) in
                                                            let name11 = (fib name10) in
                                                            let name14 = (+ name11) in
                                                            let name12 = (- n) in
                                                            let name13 = (name12 2) in
                                                            let name15 = (fib name13) in
                                                            (name14 name15))))

     |}]
;;

let%expect_test "CPS factorial" =
  test_anf
    {| let rec fack n k =
    if n=0 then k 1
    else fack (n-1) (fun p -> k (p*n)) |};
  [%expect
    {|
    (fun n -> (fun k -> let name18 = (= n) in
                        let cond24 = (name18 0) in
                        (if cond24 then (k 1) else let name19 = (- n) in
                                                   let name20 = (name19 1) in
                                                   let name23 = (fack name20) in
                                                   (name23 (fun p -> let name21 = (* p) in
                                                                     let name22 = (name21 n) in
                                                                     (k name22)))))) |}]
;;

let%expect_test "CPS fibonacci" =
  test_anf
    {| let rec fibk n k =
        if n=0 then k 0 else
        if n=1 then k 1 else
        fibk (n-1) (fun p -> fibk (n-2) (fun q -> k(p+q))) |};
  [%expect
    {|
    (fun n -> (fun k -> let name25 = (= n) in
                        let cond36 = (name25 0) in
                        (if cond36 then (k 0) else let name26 = (= n) in
                                                   let cond35 = (name26 1) in
                                                   (if cond35 then (k 1) else
                                                   let name27 = (- n) in
                                                   let name28 = (name27 1) in
                                                   let name34 = (fibk name28) in
                                                   (name34 (fun p -> let name29 = (- n) in
                                                                     let name30 = (name29 2) in
                                                                     let name33 = (fibk name30) in
                                                                     (name33 (fun q ->
                                                                     let name31 = (+ p) in
                                                                     let name32 = (name31 q) in
                                                                     (k name32)))))))))

     |}]
;;
