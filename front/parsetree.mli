(** Abstract syntax tree for MiniML, helper functions. *)

type const =
  | PConst_int of int
  (* | PConst_string of string *)
  | PConst_bool of bool
[@@deriving show { with_path = false }]

type pattern =
  | PAny
  | PVar of string
  | PTuple of pattern * pattern * pattern list
  | PConstruct of string * pattern option

val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string
val pvar : string -> pattern

(** The special type to prevent boolean blindness *)
type rec_flag =
  | Recursive
  | NonRecursive

val pp_rec_flag : Format.formatter -> rec_flag -> unit
val show_rec_flag : rec_flag -> string

type 'a list1 = 'a * 'a list

type expr =
  | EUnit
  | EConst of const
  | EVar of string
  | EIf of expr * expr * expr
  | ELam of pattern * expr
  | EApp of expr * expr
  | ETuple of expr * expr * expr list
  | ELet of rec_flag * pattern * expr * expr
  | EConstruct of string * expr option
  | EMatch of expr * (pattern * expr) list1

type value_binding = rec_flag * pattern * expr

type type_definition = {
  typedef_params : string list; (** ['a] is param in [type 'a list = ...]  *)
  typedef_name : string; (** [list] is name in [type 'a list = ...]  *)
  typedef_kind : type_kind;
}

and type_kind =
  | TKAlias of core_type  (** [t] is allias of [int] in [type t = int] *)
  | TKVariants of (string * core_type option) list1
      (** [type t = A of int | B of int -> int] *)
  (* | TKRecord of (string * core_type) list1
      * [type t = { x : int; y : int -> int }] *)

and core_type =
  | CTVar of string  (** ['a] *)
  | CTArrow of core_type * core_type  (** ['a -> 'b] *)
  | CTTuple of core_type * core_type * core_type list (** ['a * 'b] *)
  | CTConstr of core_type * string  (** ['a list] *)

type structure_item =
  | SLet of value_binding
  | SType of type_definition list1

type structure = structure_item list

val show_structure : structure -> string
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
val ele : expr -> expr -> expr
val egt : expr -> expr -> expr
val etuple : expr -> expr -> expr list -> expr
val group_lams : expr -> pattern list * expr

(* TODO (psi) : make more pretty? *)
val ematch : expr -> pattern * expr -> (pattern * expr) list -> expr
