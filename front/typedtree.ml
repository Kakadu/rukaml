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
  | V of var_info
  | Weak of binder
  | Arrow of ty * ty
  | TParam of ty * string
  | TLink of ty
  | TProd of ty * ty * ty list
  | TConstr of string * ty list
[@@deriving show { with_path = false }]

module IntMap = Map.Make (Int) [@@deriving show { with_path = false }]

type weak_table =
  { mutable map : ty Map.Make(Int).t
  ; mutable last : int
  }

let empty_table = { map = IntMap.empty; last = 0 }

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

let tarrow l r = { typ_desc = Arrow (l, r) }
let tweak t = { typ_desc = Weak t }
let tprim s = { typ_desc = TConstr (s, []) }
let tv binder ~level = { typ_desc = V { binder; var_level = level } }
let tlink t = { typ_desc = TLink t }
let tparam a t = { typ_desc = TParam (a, t) }
let tprod a b ts = { typ_desc = TProd (a, b, ts) }
let tconstr name tys = { typ_desc = TConstr (name, tys) }
let int_typ = tprim "int"
let char_typ = tprim "char"
let bool_typ = tprim "bool"
let unit_typ = tprim "unit"
let array_typ a = tparam a "array"

type pattern =
  | Tpat_var of Ident.t
  | Tpat_tuple of pattern * pattern * pattern list
  | Tpat_any
  | Tpat_constr of Ident.t * pattern option
[@@deriving show { with_path = false }]

let of_untyped_pattern =
  let rec helper = function
    | Parsetree.PVar v -> Tpat_var (Ident.of_string v)
    | PTuple (a, b, xs) -> Tpat_tuple (helper a, helper b, List.map helper xs)
    | Parsetree.PAny -> failwith "TODO (psi) : not implemented"
    | Parsetree.PConstruct _ -> failwith "TODO (psi) : not implemented"
  in
  helper
;;

type expr =
  | TUnit
  | TConst of Parsetree.const
  | TVar of string * Ident.t * ty
  | TIf of expr * expr * expr * ty
  | TLam of pattern * expr * ty
  | TApp of expr * expr * ty
  | TArray of expr list * ty
  | TTuple of expr * expr * expr list * ty
  | TLet of Parsetree.rec_flag * pattern * scheme * expr * expr
  | TMatch of expr * (pattern * expr) Parsetree.list1 * ty
  | TConstruct of string * Ident.t * ty option * ty
[@@deriving show { with_path = false }]

let rec type_of_expr = function
  | TUnit -> unit_typ
  | TConst _ -> int_typ
  | TVar (_, _, t)
  | TTuple (_, _, _, t)
  | TIf (_, _, _, t)
  | TArray (_, t)
  | TLam (_, _, t)
  | TApp (_, _, t) -> t
  | TLet (_, _, _, _, wher) -> type_of_expr wher
  | TMatch (_, _, t) -> t
  | TConstruct (_, _, _, t) -> t
;;

(** Compaction of the tree *)

let type_without_links =
  let rec helper t =
    match t.typ_desc with
    | V _ | Weak _ -> t
    | Arrow (l, r) -> tarrow (helper l) (helper r)
    | TParam (a, t) -> tparam (helper a) t
    | TLink ty -> helper ty
    | TProd (a, b, ts) -> { typ_desc = TProd (helper a, helper b, List.map helper ts) }
    | TConstr (name, tys) -> { typ_desc = TConstr (name, List.map helper tys) }
  in
  helper
;;

let compact_expr =
  let rec helper t =
    match t with
    | TUnit | TConst _ -> t
    | TVar (name, id, ty) -> TVar (name, id, type_without_links ty)
    | TIf (a, b, c, ty) -> TIf (helper a, helper b, helper c, type_without_links ty)
    | TLam (pat, e, ty) -> TLam (pat, helper e, type_without_links ty)
    | TArray (a, ty) -> TArray (List.map helper a, type_without_links ty)
    | TApp (l, r, ty) -> TApp (helper l, helper r, type_without_links ty)
    | TLet (flg, pat, S (vars, ty), e1, e2) ->
      TLet (flg, pat, S (vars, type_without_links ty), helper e1, helper e2)
    | TTuple (a, b, ts, ty) -> TTuple (helper a, helper b, List.map helper ts, ty)
    | TMatch (e, ((p1, e1), cases), ty) ->
      TMatch
        ( helper e
        , ((p1, helper e1), List.map (fun (p, e) -> p, helper e) cases)
        , type_without_links ty )
    | TConstruct (name, id, arg_ty_opt, ty) ->
      TConstruct
        (name, id, Option.map type_without_links arg_ty_opt, type_without_links ty)
  in
  helper
;;

type value_binding =
  { tvb_flag : Parsetree.rec_flag
  ; tvb_pat : pattern
  ; tvb_body : expr
  ; tvb_typ : scheme
  }

let value_binding tvb_flag tvb_pat tvb_body tvb_typ =
  { tvb_flag; tvb_pat; tvb_body; tvb_typ }
;;

type type_kind =
  | Tty_abstract of ty option
  | Tty_variants of (string * ty option) list

module TypeEnv = struct
  type constructor_entry =
    { constr_ident : Ident.t
    ; constr_name : string
    ; constr_type_ident : Ident.t
    ; constr_arg_ty : ty option
    ; constr_arity : int
    }

  type type_entry =
    { type_ident : Ident.t
    ; type_name : string
    ; type_params : binder_set
    ; type_kind : type_kind
    }

  type t =
    { env_constructors : constructor_entry Ident.Ident_map.t
    ; env_types : type_entry Ident.Ident_map.t
    ; env_values : scheme Ident.Ident_map.t
    }

  let empty =
    { env_values = Ident.Ident_map.empty
    ; env_types = Ident.Ident_map.empty
    ; env_constructors = Ident.Ident_map.empty
    }
  ;;
end

type structure = (value_binding * TypeEnv.t) list
