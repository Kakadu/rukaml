let ir_i64 = Unsigned.UInt16.of_int 11

open Unsigned.UInt32

(*node types*)
let ir_start = of_int 99
let ir_return = of_int 115
let ir_if = of_int 111
let ir_eq = of_int 270
let ir_ne = of_int 271
let ir_lt = of_int 272
let ir_ge = of_int 273
let ir_le = of_int 274
let ir_gt = of_int 275
let ir_if_true = of_int 102
let ir_if_false = of_int 103
let ir_end = of_int 109
let ir_merge2 = of_int 131179
let ir_phi2_i64 = of_int 199487
let ir_copy_i64 = of_int 2880
let ir_add_i64 = of_int 2842
let ir_sub_i64 = of_int 2843
let ir_mul_i64 = of_int 2844
let ir_div_i64 = of_int 2845
let ir_mod_i64 = of_int 2846
let ir_call1_i64 = of_int 199498
let ir_call_i64 = of_int 2890

(*flags*)
let ir_function = of_int 256
let ir_opt_folding = of_int 1048576
