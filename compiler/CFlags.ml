type stop_after_t =
  | SA_dont
  | SA_ANF

type cfg =
  { mutable out_file : string
  ; mutable input_file : string option (* mutable dump_ir : bool; *)
  ; mutable dsource : bool
  ; mutable dump_anf : bool
  ; mutable stop_after : stop_after_t
  ; mutable wrap_main_into_start : bool
  ; mutable cps_on : bool
  ; mutable call_arity : bool
  }
