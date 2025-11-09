type error =
  [ `Occurs_check
  | `No_ident of Ident.t
  | `NoVariable of string
  | `UnificationFailed of Typedtree.ty * Typedtree.ty
  | `Only_varibles_on_the_left_of_letrec
  | `Unbound_constructor of string
  | `Type_arity_mismatch of string
  | `Type_param_duplicates of string
  | `Unbound_type_variable of string
  | `Type_env_invariant_violation of string
  | `Unbound_type of string
  | `Constructor_arity_mismatch of string
  ]

val pp_error : Format.formatter -> error -> unit

val w : Parsetree.expr -> (Typedtree.expr, [> error ]) Result.t

val vb
  :  ?env:Typedtree.TypeEnv.t
  -> Typedtree.weak_table
  -> Parsetree.value_binding
  -> (Typedtree.TypeEnv.t * Typedtree.value_binding, [> error ]) Result.t

val structure
  :  ?env:Typedtree.TypeEnv.t
  -> Typedtree.weak_table
  -> Parsetree.structure
  -> (Typedtree.structure, error) Result.t
