(* https://www.khoury.northeastern.edu/home/shivers/papers/nobrainer-cps.pdf *)

(* open Miniml.Parsetree

type 'a string_map
type ds_expr (* like Parsetree.expr but every var has id*)

type error =
  [ `Free_vars_occured of int string_map
  | `Let_rec_not_allowed of ds_expr
  ]

type pat (*cps pat*)
type p (*cps program*)


val cps_conv_program : value_binding list -> (rec_flag * pat * p, error) Result.t
val cps_conv_vb : value_binding -> (rec_flag * pat * p, error) Result.t
val cps_vb_to_parsetree_vb : rec_flag * pat * p -> value_binding
val pp_error : Format.formatter -> error -> unit
val pp_vb : Format.formatter -> rec_flag * pat * p -> unit *)
