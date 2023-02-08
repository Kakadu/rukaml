(* https://matt.might.net/articles/a-normalization/ *)

open Miniml
open Typedtree

let failwiths ppf = Format.kasprintf failwith ppf

(*     <aexp> ::= NUMBER | STRING | VAR | BOOLEAN | PRIMOP
            |  (lambda (VAR ...) <exp>)

    <cexp> ::= (<aexp> <aexp> ...)
            |  (if <aexp> <exp> <exp>)

    <exp>  ::= (let ([VAR <cexp>]) <exp>)
            |  <cexp>
            |  <aexp>

 *)

type a_expr =
  | AConst of int
  | AVar of string
  | APrimitive
  | ALam of string * expr

and c_expr =
  | CApp of a_expr * a_expr
  | CIte of a_expr * expr * expr
  | CAtom of a_expr

and expr =
  | ELet of Parsetree.rec_flag * string * c_expr * expr
  | EComplex of c_expr

include struct
  open Format

  let rec pp_expr ppf = function
    | EComplex c -> pp_complex ppf c
    | ELet (NonRecursive, name, rhs, wher) ->
      fprintf
        ppf
        "@[<v>@[let %a = %a in@]@,@[%a@]@]"
        pp_print_string
        name
        pp_complex
        rhs
        pp_expr
        wher
    | ELet (Recursive, name, rhs, wher) ->
      fprintf
        ppf
        "@[let rec %a  = %a in@]@,%a"
        pp_print_string
        name
        pp_complex
        rhs
        pp_expr
        wher

  and pp_complex ppf = function
    | CAtom a -> pp_atom ppf a
    | CIte (cond, th, el) ->
      fprintf
        ppf
        "@[<v 2>@[if %a@]@,@[then %a@]@,@[else %a@]@]"
        pp_atom
        cond
        pp_expr
        th
        pp_expr
        el
    | CApp (l, r) -> fprintf ppf "@[(%a %a)@]" pp_atom l pp_atom r

  and pp_atom ppf = function
    | AConst n -> pp_print_int ppf n
    | AVar v -> fprintf ppf "%s" v
    | APrimitive -> fprintf ppf "?"
    | ALam (name, b) -> fprintf ppf "@[(fun %s -> %a)@]" name pp_expr b
  ;;
end

let pp_a_expr = pp_atom
let pp_c_expr = pp_complex

type rez =
  [ `A of a_expr
  | `C of c_expr
  | `E of expr
  ]
[@@deriving show { with_path = false }]

let expr_of_rez = function
  | `A e -> EComplex (CAtom e)
  | `C e -> EComplex e
  | `E e -> e
;;

[@@@ocaml.warnerror "-11"]

module Ident = struct
  type t = string * int

  let pp ppf (name, n) = Format.fprintf ppf "@[%s/%d@]" name n
  let of_int ?(name = "v") n = Format.asprintf "%a" pp (name, n)
end

module M : sig
  type +'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val gensym : int t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val run : 'a t -> int -> ('a -> 'c) -> 'c

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
  end
end = struct
  module type MONAD = sig
    type 'a t

    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module type MONADERROR = sig
    include MONAD

    val error : string -> 'a t
  end

  module Cont : sig
    type +'a t

    include MONADERROR with type 'a t := 'a t

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val run_cont : ('a -> 'b) -> 'a t -> 'b
  end = struct
    type ('a, 'b) cont = 'a -> 'b
    type +'a t = { cont : 'b. ('a, 'b) cont -> 'b }

    let return (x : 'a) = { cont = (fun k -> k x) }

    let ( >>= ) (x : 'a t) (f : 'a -> 'b t) : 'b t =
      { cont = (fun k -> x.cont (fun v -> (f v).cont k)) }
    ;;

    let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t = fun x f -> x >>= fun u -> return (f u)
    let error = failwith
    let run_cont f { cont } = cont f
  end

  module type READER_MONAD = sig
    type ('s, +'a) t

    val return : 'a -> ('s, 'a) t
    val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
    val ( >>| ) : ('s, 'a) t -> ('a -> 'b) -> ('s, 'b) t
    val read : ('s, 's) t
    val run : ('st, 'a) t -> 'st -> 'a
  end

  module Simple_reader : sig
    include READER_MONAD

    val read : ('s, 's) t
    val put : 's -> ('s, unit) t

    module Syntax : sig
      val ( let* ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
    end
  end = struct
    type ('st, 'a) t = 'st -> 'st * 'a

    let ( >>= ) : 's 'a 'b. ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t =
     fun x f : _ ->
      fun st ->
       let st, x = x st in
       f x st
   ;;

    let return x : _ = fun st -> st, x

    let ( >>| ) : ('s, 'a) t -> ('a -> 'b) -> ('s, 'b) t =
     fun x f st ->
      let st, x = x st in
      st, f x
   ;;

    let read : ('st, 'st) t = fun st -> st, st
    let put n : _ = fun _ -> n, ()
    let run : ('st, 'a) t -> 'st -> 'a = fun f st -> snd (f st)

    module Syntax = struct
      let ( let* ) = ( >>= )
    end
  end

  type +'a t = (int, 'a Cont.t) Simple_reader.t

  let return : 'a -> 'a t = fun x -> Simple_reader.return (Cont.return x)

  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
   fun x f ->
    (* TODO(Kakadu): Do I lose tailrec here ? *)
    Simple_reader.( >>= ) x (Cont.run_cont f)
 ;;

  let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t =
   fun x f -> Simple_reader.( >>| ) x (fun c -> Cont.( >>| ) c f)
 ;;

  let run m st f = Simple_reader.run m st |> Cont.run_cont f

  let gensym : int t =
    let open Simple_reader.Syntax in
    let* n = Simple_reader.read in
    let* () = Simple_reader.put (n + 1) in
    return n
  ;;

  module Syntax = struct
    let ( let* ) = ( >>= )
    let return = return
  end
end

let complex_to_atomic cexp =
  let open M.Syntax in
  let* name = M.gensym in
  let ident = Ident.of_int name in
  return ([ ident, cexp ], AVar ident)
;;

let rec expr_to_atomic exp k : expr M.t =
  let open M.Syntax in
  match exp with
  | EComplex (CAtom c) -> k c
  | EComplex c ->
    let* prefix, a = complex_to_atomic c in
    let init = k a in
    List.fold_right
      (fun (name, rhs) acc ->
        let* acc = acc in
        return @@ ELet (NonRecursive, name, rhs, acc))
      prefix
      init
  | ELet (flg, name, rhs, wher) ->
    let* wher = expr_to_atomic wher k in
    return (ELet (flg, name, rhs, wher))
;;

let rec normalize : Typedtree.expr -> rez M.t =
  let open M in
  let open M.Syntax in
  let return_c x = M.return (`C x) in
  let return_e x = M.return (`E x) in
  let alam name b = `A (ALam (name, b)) in
  function
  | TConst n -> return (`A (AConst n))
  | TLam (name, body, _) ->
    let* b = normalize body in
    return (alam name (expr_of_rez b))
  | TIf (cond, th, el, _) ->
    let* cond = normalize cond in
    let* th = normalize th in
    let* el = normalize el in
    (match cond with
     | `A cond -> return_c @@ CIte (cond, expr_of_rez th, expr_of_rez el)
     | `C cond ->
       let* name = M.gensym in
       let ident = string_of_int name in
       let ite = CIte (AVar ident, expr_of_rez th, expr_of_rez el) in
       return_e (ELet (NonRecursive, ident, cond, EComplex ite))
     | `E e ->
       let* rez =
         expr_to_atomic e (fun a ->
           return @@ EComplex (CIte (a, expr_of_rez th, expr_of_rez el)))
       in
       return_e rez)
  | TLet (flg, pat, _, body, wher) ->
    (* normalize body (fun body -> ELet (flg, pat, EComplex body, normalize wher k)) *)
    assert false
  | TVar ("=", _) -> return (`A (AVar "="))
  | TVar (v, _) -> return (`A (AVar v))
  | TApp (l, r, _) ->
    let* l = normalize l in
    let* r = normalize r in
    let complex_to_atomic2 expr k =
      let* pref1, l = complex_to_atomic expr in
      let* (init : expr) = k l in
      let (_ : (string * c_expr) list) = pref1 in
      List.fold_right
        (fun (name, rhs) acc -> ELet (NonRecursive, name, rhs, acc))
        pref1
        init
      |> return
    in
    (match l, r with
     | `A l, `A r -> return_c (CApp (l, r))
     | `A l, `C r ->
       complex_to_atomic2 r (fun r -> return @@ EComplex (CApp (l, r))) >>= return_e
     | `C l, `A r ->
       complex_to_atomic2 l (fun l -> return @@ EComplex (CApp (l, r))) >>= return_e
     | `C l, `C r ->
       complex_to_atomic2 l (fun l ->
         complex_to_atomic2 r (fun r -> return @@ EComplex (CApp (l, r))))
       >>= return_e
     | `E l, `C r ->
       complex_to_atomic2 r (fun r ->
         expr_to_atomic l (fun l -> return @@ EComplex (CApp (l, r))))
       >>= return_e
     | `C l, `E r ->
       complex_to_atomic2 l (fun l ->
         expr_to_atomic r (fun r -> return @@ EComplex (CApp (l, r))))
       >>= return_e
     | `E l, `A r ->
       expr_to_atomic l (fun l -> return @@ EComplex (CApp (l, r))) >>= return_e
     | `A l, `E r ->
       expr_to_atomic r (fun r -> return @@ EComplex (CApp (l, r))) >>= return_e
     | `E l, `E r ->
       expr_to_atomic l (fun l ->
         let* e = expr_to_atomic r (fun r -> return @@ EComplex (CApp (l, r))) in
         return e)
       >>= return_e)
  | s -> failwiths "Not supported: %a" Typedtree.pp_hum s
;;

let normalize_term t = M.run (normalize t) 0 Fun.id |> expr_of_rez

let%expect_test " " =
  let text = {| let rec fac = fun n -> if n=0 then 1 else n * (fac (n-1))|} in
  let ( let* ) x f = Result.bind x f in
  (let stru = Miniml.Parsing.parse_vb_exn text in
   let* { tvb_body = typed; _ } = Inferencer.vb stru in
   let anf = normalize_term typed in
   Format.printf "%a\n%!" pp_expr anf;
   Result.Ok ())
  |> ignore;
  [%expect
    {|
    (fun n -> let v/0 = (= n) in
              let v/5 = (v/0 0) in
              if v/5
                then 1
                else let v/3 = (* n) in
                     let v/1 = (- n) in
                     let v/2 = (v/1 1) in
                     let v/4 = (fac v/2) in
                     (v/3 v/4)) |}]
;;
