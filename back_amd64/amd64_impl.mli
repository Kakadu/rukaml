val set_verbose : bool -> unit

val codegen :
  ?compile_only:bool ->
  Compile_lib.Rkmi.repr ->
  (Miniml.Parsetree.rec_flag * Miniml.Ident.t * Compile_lib.ANF.expr) list ->
  string ->
  (unit, 'b) result
