open CPSLang

val call_arity_anal
  :  ?disable_dead_code_elem:bool
  -> OneACPS.cps_vb list
  -> MACPS.cps_vb list
val call_arity_anal_debug
  :  ?disable_dead_code_elem:bool
  -> OneACPS.cps_vb list
  -> MACPS.cps_vb list
