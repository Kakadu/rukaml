type error =
  [ `Occurs_check
  | `No_ident of Ident.t
  | `NoVariable of string
  | `UnificationFailed of Typedtree.ty * Typedtree.ty
  | `Only_varibles_on_the_left_of_letrec
  ]

val pp_error : Format.formatter -> error -> unit
val w : Parsetree.expr -> (Typedtree.expr, [> error ]) Result.t

module Type_env : sig
  open Typedtree

  type t = Typedtree.scheme Ident.Ident_map.t

  val pp : Format.formatter -> t -> unit
  val empty : t
  val extend : varname:string -> Ident.t -> scheme -> t -> t
  val extend_string : string -> scheme -> t -> t
  val extend_by_ident : Ident.t -> scheme -> t -> t
  val ident_of_string_exn : string -> t -> Ident.t
  val find_exn : Ident.t -> t -> scheme
  val find_by_string_exn : string -> t -> scheme
  val free_vars : t -> Var_set.t
end

val start_env : Type_env.t

val vb
  :  ?env:Type_env.t
  -> Parsetree.value_binding
  -> (Type_env.t * Typedtree.value_binding, [> error ]) Result.t

val structure
  :  ?env:Type_env.t
  -> Parsetree.structure
  -> (Typedtree.structure, [> error ]) Result.t
