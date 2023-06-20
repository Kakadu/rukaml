type binder = int [@@deriving show { with_path = false }]

module Var_set = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = Var_set.t [@@deriving show { with_path = false }]

type var_info =
  { binder : binder
  ; mutable var_level : int
  }
[@@deriving show { with_path = false }]

type ty = { mutable typ_desc : type_desc }

and type_desc =
  | Prim of string
  | V of var_info
  | Arrow of ty * ty
  | TLink of ty
  | TProd of ty * ty * ty list
[@@deriving show { with_path = false }]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

let tarrow l r = { typ_desc = Arrow (l, r) }
let tprim s = { typ_desc = Prim s }
let tv binder ~level = { typ_desc = V { binder; var_level = level } }
let tlink t = { typ_desc = TLink t }
let tprod a b ts = { typ_desc = TProd (a, b, ts) }
let int_typ = tprim "int"
let bool_typ = tprim "bool"
let unit_typ = tprim "unit"

type pattern = string [@@deriving show { with_path = false }]

type expr =
  | TUnit
  | TConst of Parsetree.const
  | TVar of string * ty
  | TIf of expr * expr * expr * ty
  | TLam of pattern * expr * ty
  | TApp of expr * expr * ty
  | TTuple of expr * expr * expr list * ty
  | TLet of Parsetree.rec_flag * pattern * scheme * expr * expr
[@@deriving show { with_path = false }]

let rec type_of_expr = function
  | TUnit -> unit_typ
  | TConst _ -> int_typ
  | TTuple (_, _, _, t) | TVar (_, t) | TIf (_, _, _, t) | TLam (_, _, t) | TApp (_, _, t)
    -> t
  | TLet (_, _, _, _, wher) -> type_of_expr wher
;;

(** Compaction of the tree *)

let type_without_links =
  let rec helper t =
    match t.typ_desc with
    | Prim _ | V _ -> t
    | Arrow (l, r) -> tarrow (helper l) (helper r)
    | TLink ty -> helper ty
    | TProd (a, b, ts) -> { typ_desc = TProd (helper a, helper b, List.map helper ts) }
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
    | TTuple (a, b, ts, ty) -> TTuple (helper a, helper b, List.map helper ts, ty)
  in
  helper
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
