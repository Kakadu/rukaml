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
  | V of var_info
  | Weak of binder
  | Arrow of ty * ty
  | TLink of ty
  | TProd of ty * ty * ty list
  | TConstr of ty list * string
  (** [ int ] is [ TConstr ([], "int") ] ;; [ int array ] is [ TConstr ([ TConst ("int", []) ], "array") ] etc. *)

module IntMap : Map.S with type key = int

type weak_table =
  { mutable map : ty IntMap.t
  ; mutable last : int
  }

val empty_table : weak_table
val pp_ty : Format.formatter -> ty -> unit
val pp_type_desc : Format.formatter -> type_desc -> unit
val show_ty : ty -> string
val show_type_desc : type_desc -> string

type scheme = S of binder_set * ty

val pp_scheme : Format.formatter -> scheme -> unit
val show_scheme : scheme -> string
val tarrow : ty -> ty -> ty
val tprim : string -> ty
val tweak : binder -> ty
val tparam : ty -> string -> ty
val tv : binder -> level:int -> ty
val tlink : ty -> ty
val tprod : ty -> ty -> ty list -> ty
val tconstr : ty list -> string -> ty
val int_typ : ty
val char_typ : ty
val bool_typ : ty
val unit_typ : ty
val string_typ : ty
val array_typ : ty -> ty
val list_typ : ty -> ty
val in_channel_typ : ty
val out_channel_typ : ty
val format3_typ : arg_ty:ty -> dest_ty:ty -> out_ty:ty -> ty

type pattern =
  | Tpat_unit
  | Tpat_const of Parsetree.const
  | Tpat_var of Ident.t
  | Tpat_tuple of pattern * pattern * pattern list
  | Tpat_any
  | Tpat_constr of Ident.t * pattern option

val show_pattern : pattern -> string
val pp_pattern : Format.formatter -> pattern -> unit
val of_untyped_pattern : Parsetree.pattern -> pattern

type def_kind =
  | User
  | Builtin of string * int

type expr =
  | TUnit
  | TConst of Parsetree.const (** Contants *)
  | TVar of string * Ident.t * def_kind * ty
  | TIf of expr * expr * expr * ty (** if ... then ... else ... *)
  | TLam of pattern * expr * ty (** fun ... -> ... *)
  | TApp of expr * expr * ty (** Application f x *)
  | TArray of expr list * ty (** Array [| ... |]*)
  | TTuple of expr * expr * expr list * ty (** Tuple (a,b,...,_) as a tuple (a,b,...) *)
  | TLet of Parsetree.rec_flag * pattern * scheme * expr * expr
  (** let rec? .. = ... in ... *)
  | TMatch of expr * (pattern * expr) Parsetree.list1 * ty
  | TFormat of string * ty

val type_of_expr : expr -> ty
val type_without_links : ty -> ty
val compact_expr : expr -> expr
val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_binder_set : Format.formatter -> binder_set -> unit
val show_binder_set : binder_set -> string
val pp_binder : Format.formatter -> binder -> unit
val show_binder : binder -> string

type value_binding =
  { tvb_flag : Parsetree.rec_flag
  ; tvb_pat : pattern
  ; tvb_body : expr
  ; tvb_typ : scheme
  }

type type_kind =
  | Tty_abstract of ty option
  | Tty_variants of (Ident.t * ty option) list

type type_declaration =
  { tty_ident : Ident.t
  ; tty_params : binder_set (* TODO:  replace it with Ident.t list or smth like that *)
  ; tty_kind : type_kind
  }

type constructor_info =
  { constr_ident : Ident.t
  ; constr_type_ident : Ident.t
  ; constr_arg : ty option
  }

type structure_item =
  | Tstr_value of value_binding
  | Tstr_type of type_declaration

type structure = structure_item list

val value_binding : Parsetree.rec_flag -> pattern -> expr -> scheme -> value_binding

module TypeEnv : sig
  type t =
    { env_constructors : constructor_info Ident.String_map.t
    ; env_types : type_declaration Ident.Ident_map.t
    ; env_values : (scheme * def_kind) Ident.Ident_map.t
    }

  val empty : t
  val add_type : t -> type_declaration -> t
  val typ_unit : type_declaration
  val typ_int : type_declaration
  val typ_bool : type_declaration
  val typ_array : type_declaration
  val env_with_base_types : t

  module TypeList : sig
    val typ_list : type_declaration
    val constr_nil : constructor_info
    val constr_cons : constructor_info
  end
end
