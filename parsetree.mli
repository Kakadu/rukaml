(** Abstract syntax tree for MiniML, helper functions. *)

(** Patterns *)
type pattern =
  | PVar of string (** Variable *)
  | PTuple of pattern * pattern * pattern list (** Tuples *)
(* TODO: add wildcard patterns, etc... *)

val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string
val pvar : string -> pattern

(** The special type to prevent boolean blindness *)
type rec_flag =
  | Recursive
  | NonRecursive

val pp_rec_flag : Format.formatter -> rec_flag -> unit
val show_rec_flag : rec_flag -> string

type const =
  | PConst_int of int (** Contant 42 *)
  (* | PConst_string of string *)
  | PConst_bool of bool
[@@deriving show]

type expr =
  | EUnit
  | EConst of const
  | EVar of string
  | EIf of expr * expr * expr (** if ... then ... else ... *)
  | ELam of pattern * expr (** fun ... -> ... *)
  | EApp of expr * expr (** Application f x *)
  | ETuple of expr * expr * expr list
  | ELet of rec_flag * pattern * expr * expr (** let rec? .. = ... in ...  *)

type value_binding = rec_flag * pattern * expr
type structure_item = value_binding
type structure = structure_item list

val const_int : int -> const
val const_bool : bool -> const

(* val pp_value_binding : Format.formatter -> value_binding -> unit *)
val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val econst : const -> expr
val eunit : expr
val evar : string -> expr
val elam : pattern -> expr -> expr
val eapp : expr -> expr list -> expr
val eapp1 : expr -> expr -> expr
val elet : ?isrec:rec_flag -> pattern -> expr -> expr -> expr
val eite : expr -> expr -> expr -> expr
val emul : expr -> expr -> expr
val eadd : expr -> expr -> expr
val esub : expr -> expr -> expr
val eeq : expr -> expr -> expr
val elt : expr -> expr -> expr
val egt : expr -> expr -> expr
val etuple : expr -> expr -> expr list -> expr
val group_lams : expr -> pattern list * expr
