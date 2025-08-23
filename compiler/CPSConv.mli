(* https://www.khoury.northeastern.edu/home/shivers/papers/nobrainer-cps.pdf *)
open Frontend.Parsetree
open CPSLang.OneACPS

type 'a string_map
type ds_expr (* like Parsetree.expr but every var has id*)

type error =
  [ `Free_vars_occured of int string_map
  | `Let_rec_not_allowed of ds_expr
  ]

val pp_error : Format.formatter -> error -> unit
val cps_conv_program : value_binding list -> (cps_vb, error) Result.t
val cps_conv_vb : value_binding -> (cps_vb, error) Result.t
