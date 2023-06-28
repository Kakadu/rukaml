type error =
  [ `Occurs_check
  | `NoVariable of string
  | `UnificationFailed of Typedtree.ty * Typedtree.ty
  | `Only_varibles_on_the_left_of_letrec
  ]

val pp_error : Format.formatter -> error -> unit
val w : Parsetree.expr -> (Typedtree.expr, [> error ]) Result.t
val start_env : (string * Typedtree.scheme) list

(* TODO(Kakadu): implement properly *)
val vb
  :  ?env:(string * Typedtree.scheme) list
  -> Parsetree.value_binding
  -> (Typedtree.value_binding, [> error ]) Result.t

val structure
  :  ?env:(string * Typedtree.scheme) list
  -> Parsetree.structure
  -> (Typedtree.structure, [> error ]) Result.t
