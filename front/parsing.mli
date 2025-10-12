val set_logging : bool -> unit

type dispatch =
  { prio : dispatch -> Parsetree.expr Angstrom.t
  ; expr_basic : dispatch -> Parsetree.expr Angstrom.t
  ; expr_long : dispatch -> Parsetree.expr Angstrom.t
  ; expr : dispatch -> Parsetree.expr Angstrom.t
  }

val is_digit : char -> bool
val to_digit : char -> int
val digit : int Angstrom.t
val number : int Angstrom.t
val is_keyword : string -> bool
val is_char_valid_for_name : char -> bool
val string : string -> string Angstrom.t
val pattern : Parsetree.pattern Angstrom.t
val core_type : Parsetree.core_type Angstrom.t
val keyword : string -> unit Angstrom.t

(** [ ^[a-zA-Z][a-zA-Z0-9_]*$ ] *)
val ident : string Angstrom.t

(**[ ^[A-Z][a-zA-Z0-9_]*$ ]*)
val constructor_name : string Angstrom.t

(** [ ^[a-z_][a-zA-Z0-9_]*$ ] *)
val type_name : string Angstrom.t

(** [ ^'[a-zA-Z][a-zA-Z0-9_]*$ ] *)
val type_param_name : string Angstrom.t

(** [ ^[A-Z][a-zA-Z0-9_]*$ ] *)
val constructor_name : string Angstrom.t

val prio
  :  Parsetree.expr Angstrom.t
  -> ('a Angstrom.t * (Parsetree.expr -> Parsetree.expr -> Parsetree.expr)) list array
  -> Parsetree.expr Angstrom.t

val letdef
  :  Parsetree.expr Angstrom.t
  -> (Parsetree.rec_flag * Parsetree.pattern * Parsetree.expr) Angstrom.t

val letdef0
  :  Parsetree.expr Angstrom.t
  -> (Parsetree.rec_flag * Parsetree.pattern * Parsetree.expr) Angstrom.t Angstrom.t
       Angstrom.t
       Angstrom.t

val pack : dispatch
val value_binding : Parsetree.value_binding Angstrom.t
val value_bindings : Parsetree.value_binding list Angstrom.t
val structure : Parsetree.structure_item list Angstrom.t

type error = [ `Parse_error of string ]

val pp_error : Format.formatter -> error -> unit
val parse : string -> (Parsetree.expr, [> error ]) result
val parse_pack : (dispatch -> 'a Angstrom.t) -> string -> ('a, string) result
val parse_pat_exn : string -> Parsetree.pattern
val parse_vb_exn : string -> Parsetree.value_binding
val parse_value_bindings : string -> (Parsetree.value_binding list, [> error ]) result
val parse_structure : string -> (Parsetree.structure_item list, [> error ]) result
