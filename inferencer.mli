type error =
  [ `Occurs_check
  | `NoVariable of string
  | `UnificationFailed of Typedtree.ty * Typedtree.ty
  ]

val pp_error : Format.formatter -> error -> unit
val w : Parsetree.expr -> (Typedtree.expr, [> error ]) Result.t

(* TODO(Kakadu): implement properly *)
val vb : Parsetree.value_binding -> (Typedtree.value_binding, [> error ]) Result.t
