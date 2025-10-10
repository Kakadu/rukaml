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
(* | PRecord of (string * pattern) list *)

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
  | EConst of const (** Constant *)
  | EVar of string (** Variable *)
  | EIf of expr * expr * expr (** if ... then ... else ... *)
  | ELam of pattern * expr (** fun ... -> ... *)
  | EApp of expr * expr (** Application f x *)
  | ETuple of expr * expr * expr list
  | ELet of rec_flag * pattern * expr * expr (** let rec? .. = ... in ...  *)
  | EConstruct of string * expr option (** ConstructorName(expr) *)
  | EMatch of expr * (pattern * expr) list1 (** match expr with ... *)

type value_binding = rec_flag * pattern * expr (* let rec? .. = ... *)

type type_declaration =
  { typedef_params : string list (** ['a] is param in [type 'a list = ...]  *)
  ; typedef_name : string (** [list] is name in [type 'a list = ...]  *)
  ; typedef_kind : type_kind
  }

and type_kind =
  | KAbstract of core_type option
  (** [t] is an abstract when it is defined as [type t] or [type t = x]. *)
  | KVariants of (string * core_type option) list1 (** [type t = Some of int | None]  *)
(* TODO (psi) : *)
(* | KRecord of (string * core_type) list *)

and core_type =
  | CTVar of string (** ['a, 'b] are type variables in [type ('a, 'b) ty = ... ] *)
  | CTArrow of core_type * core_type (** ['a -> 'b] *)
  | CTTuple of core_type * core_type * core_type list (** [('a * 'b * 'c)] *)
  | CTConstr of string * core_type list (** [int], [('t, int) list], ['a option] etc. *)

type structure_item =
  | SValue of value_binding (** [let x = ...] *)
  | SType of type_declaration list1 (** [type x = ...] *)

type structure = structure_item list

val show_structure : structure -> string
val const_int : int -> const
val const_bool : bool -> const

(* val pp_value_binding : Format.formatter -> value_binding -> unit *)
val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_core_type : Format.formatter -> core_type -> unit
val show_core_type : core_type -> string
val econst : const -> expr
val eunit : expr
val evar : string -> expr
val elam : pattern -> expr -> expr
val eapp : expr -> expr list -> expr
val ematch : expr -> pattern * expr -> (pattern * expr) list -> expr
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
