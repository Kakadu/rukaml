open Frontend

type apat = APname of Frontend.Ident.t

val pp_apat : Format.formatter -> apat -> unit

type imm_expr =
  | AUnit
  | AConst of Frontend.Parsetree.const
  | AVar of Frontend.Ident.t
  | APrimitive of string * int
  | ATuple of imm_expr * imm_expr * imm_expr list
  | AConstruct of int * imm_expr list
  | AArray of imm_expr list
  | ALam of apat * expr

and c_expr =
  | CApp of imm_expr * imm_expr * imm_expr list
  | CIte of c_expr * expr * expr
  | CAtom of imm_expr

and expr =
  | ELet of Frontend.Parsetree.rec_flag * Frontend.Typedtree.pattern * c_expr * expr
  | EComplex of c_expr

type vb = Parsetree.rec_flag * Ident.t * expr

type stru_item =
  | ANF_vb of vb
  | ANF_match of (Parsetree.const * Ident.t)
  | ANF_eval of expr

type stru = stru_item list

val show_c_expr : c_expr -> string
val pp_a : Format.formatter -> imm_expr -> unit
val pp_c : Format.formatter -> c_expr -> unit
val pp : Format.formatter -> expr -> unit
val pp_stru : Format.formatter -> stru -> unit
val is_infix_binop : string -> bool
val group_abstractions : expr -> apat list * expr
val simplify_stru : stru -> stru
val anf : Typedtree.expr -> expr
val anf_stru : Typedtree.structure_item list -> stru

(** Gensym stuff *)
val anf_pat
  :  Typedtree.pattern
  -> ?kbefore:(Ident.t -> expr -> expr)
  -> (Ident.t -> expr)
  -> expr

val reset_gensym : unit -> unit
val gensym : unit -> int
val gensym_s : ?prefix:string -> unit -> string
val gensym_id : ?prefix:string -> unit -> Frontend.Ident.t

(** Config stuff *)

val disable_arity_inline : unit -> unit
val disable_cmp_into_if_inline : unit -> unit
val set_logging : bool -> unit
