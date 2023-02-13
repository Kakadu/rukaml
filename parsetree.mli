(** Abstract syntax tree for MiniML, helper functions. *)

(** For now only variable patterns are allowed. *)
type pattern = PVar of string (** *)
(** TODO: add wildcard patterns, pairs, etc... *)

val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string
val pvar : string -> pattern

(** The special type to prevent boolean blindness *)
type rec_flag =
  | Recursive
  | NonRecursive

val pp_rec_flag : Format.formatter -> rec_flag -> unit
val show_rec_flag : rec_flag -> string

type expr =
  | EConst of int (** Contant 42 *)
  | EVar of string
  | EIf of expr * expr * expr (** if ... then ... else ... *)
  | ELam of pattern * expr (** fun ... -> ... *)
  | EApp of expr * expr (** Application f x *)
  | ELet of rec_flag * pattern * expr * expr (** let rec? .. = ... in ...  *)

type value_binding = rec_flag * pattern * expr
type structure_item = value_binding
type structure = structure_item list

(* val pp_value_binding : Format.formatter -> value_binding -> unit *)
val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val econst : int -> expr
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
val group_lams : expr -> string list * expr
