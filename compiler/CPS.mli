(* https://www.khoury.northeastern.edu/home/shivers/papers/nobrainer-cps.pdf *)
open Frontend.Parsetree
type 'a string_map
type ds_expr (* like Parsetree.expr but every var has id*)

type error =
  [ `Free_vars_occured of int string_map
  | `Let_rec_not_allowed of ds_expr
  ]

type cps1_vb (* one-arg cps lang vb*)

val cps_conv_program : value_binding list -> (cps1_vb, error) Result.t
val cps_conv_vb : value_binding -> (cps1_vb, error) Result.t
val cps1_vb_to_parsetree_vb : cps1_vb -> value_binding
val pp_error : Format.formatter -> error -> unit
val pp_cps1_vb : Format.formatter -> cps1_vb -> unit

type cpsm_vb (* multi-arg cps lang vb*)

val cpsm_vb_to_parsetree_vb : cpsm_vb -> value_binding
val pp_cpsm_vb : Format.formatter -> cpsm_vb -> unit
val call_arity_anal : cps1_vb -> cpsm_vb
