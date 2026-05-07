val codegen
  :  ?wrap_main_into_start:bool
  -> ('a * Frontend.Ident.t * Compile_lib.ANF.expr) list
  -> string
  -> (unit, 'b) result
