type binder = int [@@deriving show { with_path = false }]

module Var_set = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = Var_set.t [@@deriving show { with_path = false }]

type ty = { mutable typ_desc : type_desc }

and type_desc =
  | Prim of string
  | V of binder
  | Arrow of ty * ty
  | TLink of ty
[@@deriving show { with_path = false }]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

let tarrow l r = { typ_desc = Arrow (l, r) }
let tprim s = { typ_desc = Prim s }
let tv v = { typ_desc = V v }
let tlink t = { typ_desc = TLink t }
let int_typ = tprim "int"
let bool_typ = tprim "bool"
let unit_typ = tprim "unit"

type pattern = string [@@deriving show { with_path = false }]

type expr =
  | TConst of int (** Contant 42 *)
  | TVar of string * ty
  | TIf of expr * expr * expr * ty (** if ... then ... else ... *)
  | TLam of pattern * expr * ty (** fun ... -> ... *)
  | TApp of expr * expr * ty (** Application f x *)
  | TLet of Parsetree.rec_flag * pattern * scheme * expr * expr
      (** let rec? .. = ... in ...  *)
[@@deriving show { with_path = false }]

let rec type_of_expr = function
  | TConst _ -> int_typ
  | TVar (_, t) | TIf (_, _, _, t) | TLam (_, _, t) | TApp (_, _, t) -> t
  | TLet (_, _, _, _, wher) -> type_of_expr wher
;;

(** Compaction of the tree *)

let type_without_links =
  let rec helper t =
    match t.typ_desc with
    | Prim _ | V _ -> t
    | Arrow (l, r) -> tarrow (helper l) (helper r)
    | TLink ty -> helper ty
  in
  helper
;;

let compact_expr =
  let rec helper t =
    match t with
    | TConst _ -> t
    | TVar (name, ty) -> TVar (name, type_without_links ty)
    | TIf (a, b, c, ty) -> TIf (helper a, helper b, helper c, type_without_links ty)
    | TLam (pat, e, ty) -> TLam (pat, helper e, type_without_links ty)
    | TApp (l, r, ty) -> TApp (helper l, helper r, type_without_links ty)
    | TLet (flg, pat, S (vars, ty), e1, e2) ->
      TLet (flg, pat, S (vars, type_without_links ty), helper e1, helper e2)
  in
  helper
;;

let pp_typ_hum =
  let open Format in
  let rec pp_typ ppf t =
    match t.typ_desc with
    | Prim s -> fprintf ppf "%s" s
    | V n -> fprintf ppf "'_%d" n
    | Arrow (l, r) -> fprintf ppf "(%a -> %a)" pp_typ l pp_typ r
    | TLink ty -> pp_typ ppf ty
  in
  pp_typ
;;

let pp_hum =
  let open Format in
  let rec expr ppf = function
    | TConst n -> fprintf ppf "%d" n
    | TVar (name, _) -> fprintf ppf "%s" name
    | TIf (cond, th, el, _) ->
      fprintf ppf "if %a then %a else %a" expr cond expr th expr el
    | TLam (pat, e, _) -> fprintf ppf "fun %a -> %a" pp_pat pat expr e
    | TApp (TApp (TVar ("+", _), l, _), r, _) -> fprintf ppf "(%a + %a)" expr l expr r
    | TApp (TApp (TVar ("*", _), l, _), r, _) -> fprintf ppf "(%a * %a)" expr l expr r
    | TApp (TApp (TVar ("-", _), l, _), r, _) -> fprintf ppf "(%a - %a)" expr l expr r
    | TApp (TApp (TVar ("=", _), l, _), r, _) -> fprintf ppf "(%a = %a)" expr l expr r
    | TApp (l, r, _) -> fprintf ppf "(%a %a)" expr l expr r
    | TLet (Parsetree.Recursive, pat, S (_vars, ty), rhs, wher) ->
      fprintf ppf "let rec %a : %a = %a in %a" pp_pat pat pp_typ ty expr rhs expr wher
    | TLet (NonRecursive, pat, S (_vars, ty), rhs, wher) ->
      fprintf ppf "@[let %a : %a = %a in@]@,%a" pp_pat pat pp_typ ty expr rhs expr wher
  and pp_typ = pp_typ_hum
  and pp_pat ppf s = fprintf ppf "%s" s in
  fun ppf e -> fprintf ppf "@[<v>%a@]" expr e
;;

type value_binding =
  { tvb_flag : Parsetree.rec_flag
  ; tvb_pat : Parsetree.pattern
  ; tvb_body : expr
  ; tvb_typ : scheme
  }

type structure_item = value_binding
type structure = structure_item list

let value_binding tvb_flag tvb_pat tvb_body tvb_typ =
  { tvb_flag; tvb_pat; tvb_body; tvb_typ }
;;

let pp_pattern ppf = function
  | Parsetree.PVar s -> Format.fprintf ppf "%s" s
;;

let pp_vb_hum ppf { tvb_flag; tvb_pat; tvb_body; tvb_typ } =
  Format.fprintf
    ppf
    "@[let %s%a: @[%a@] =%a@]"
    (match tvb_flag with
     | Recursive -> "rec "
     | NonRecursive -> "")
    pp_pattern
    tvb_pat
    pp_typ_hum
    (match tvb_typ with
     | S (_, typ) -> typ)
    pp_hum
    tvb_body
;;

let pp_stru ppf stru =
  Format.open_vbox 0;
  Format.pp_print_list pp_vb_hum ppf stru;
  Format.close_box ()
;;
