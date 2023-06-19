type binder = int

module Var_set : sig
  include Stdlib.Set.S with type elt := binder

  val pp : Format.formatter -> t -> unit
end

type binder_set = Var_set.t

type var_info =
  { binder : binder
  ; mutable var_level : int
  }

type ty = { mutable typ_desc : type_desc }

and type_desc =
  | Prim of string
  | V of var_info
  | Arrow of ty * ty
  | TLink of ty
  | TProd of ty * ty * ty list

val pp_ty : Format.formatter -> ty -> unit
val pp_type_desc : Format.formatter -> type_desc -> unit
val show_ty : ty -> string
val show_type_desc : type_desc -> string

type scheme = S of binder_set * ty

val pp_scheme : Format.formatter -> scheme -> unit
val show_scheme : scheme -> string
val tarrow : ty -> ty -> ty
val tprim : string -> ty
val tv : binder -> level:int -> ty
val tlink : ty -> ty
val tprod : ty -> ty -> ty list -> ty
val int_typ : ty
val bool_typ : ty
val unit_typ : ty

type pattern = string

val show_pattern : pattern -> string

type expr =
  | TUnit
  | TConst of Parsetree.const (** Contants *)
  | TVar of pattern * ty
  | TIf of expr * expr * expr * ty (** if ... then ... else ... *)
  | TLam of pattern * expr * ty (** fun ... -> ... *)
  | TApp of expr * expr * ty (** Application f x *)
  | TTuple of expr * expr * expr list * ty (** Tuple (a,b,...,_) as a tuple (a,b,...) *)
  | TLet of Parsetree.rec_flag * pattern * scheme * expr * expr
      (** let rec? .. = ... in ...  *)

val type_of_expr : expr -> ty
val type_without_links : ty -> ty
val compact_expr : expr -> expr

type value_binding =
  { tvb_flag : Parsetree.rec_flag
  ; tvb_pat : Parsetree.pattern
  ; tvb_body : expr
  ; tvb_typ : scheme
  }

type structure_item = value_binding
type structure = structure_item list

val value_binding
  :  Parsetree.rec_flag
  -> Parsetree.pattern
  -> expr
  -> scheme
  -> value_binding

val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_binder_set : Format.formatter -> binder_set -> unit
val show_binder_set : binder_set -> string
val pp_binder : Format.formatter -> binder -> unit
val show_binder : binder -> string
