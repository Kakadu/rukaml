type cfg

val frontend :
  cfg ->
  ( unit,
    [ `NoVariable of string
    | `No_ident of Miniml.Ident.t
    | `Occurs_check
    | `Only_varibles_on_the_left_of_letrec
    | `Parse_error of string
    | `UnificationFailed of Miniml.Typedtree.ty * Miniml.Typedtree.ty ] )
  result

val cfg : cfg

val print_errors :
  [< `NoVariable of string
  | `No_ident of Miniml.Ident.t
  | `Occurs_check
  | `Only_varibles_on_the_left_of_letrec
  | `Parse_error of string
  | `UnificationFailed of Miniml.Typedtree.ty * Miniml.Typedtree.ty ] ->
  unit
