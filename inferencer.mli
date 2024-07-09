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
  type t = Typedtree.scheme Ident.Ident_map.t
end

val start_env : Type_env.t

val vb
  :  ?env:Type_env.t
  -> Parsetree.value_binding
  -> (Typedtree.value_binding, [> error ]) Result.t

val structure
  :  ?env:Type_env.t
  -> Parsetree.structure
  -> (Typedtree.structure, [> error ]) Result.t
