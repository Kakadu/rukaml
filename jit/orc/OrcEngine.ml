(** ORC implementation of the [JIT_engine_lib.JITEngine.OnlineEngine]
    (and [BackEngine]) contract.

    The low-level externals — [BackORC] — remain an internal detail of
    [jit/orc]: the driver sees only this module (through the annotated
    contract). [create] also brings up the rukaml runtime (absolute
    symbols), which the driver used to call explicitly. *)
include (
struct
  type t = unit

  let name = "orc"
  let create = BackORC.create

  let load () ir =
    match IR.format ir with
    | IR.TextLL -> BackORC.update (IR.payload ir)
    | IR.Bitcode -> failwith "orc: Bitcode payload is not supported yet"
  ;;

  let load_delta () ir =
    match IR.format ir with
    | IR.TextLL -> BackORC.add_delta (IR.payload ir)
    | IR.Bitcode -> failwith "orc: Bitcode payload is not supported yet"
  ;;

  let run () symbol = BackORC.run symbol
  let close = BackORC.destroy
  let runtime_symbols = BackORC.runtime_syms ()
  let symbol_address () symbol = BackORC.lookup_address symbol
  let bind_tables () num_funcs = BackORC.bind_tables num_funcs
  let install () ~slot ~symbol = BackORC.install_slot slot symbol

  let next_event () ~timeout_ms =
    let code = BackORC.next_event timeout_ms in
    if code >= 0
    then JIT_engine_lib.JITEngine.Hot code
    else if code = -2
    then JIT_engine_lib.JITEngine.Stop
    else JIT_engine_lib.JITEngine.Timeout
  ;;

  let stop_events () = BackORC.stop_events ()
  let read_arg_profile () ~func_id ~arity = BackORC.read_arg_profile func_id arity
  let read_site_profile () ~site_id = BackORC.read_site_profile site_id
  let read_counts () ~func_id = BackORC.read_counts func_id
  let reset_func_profile () ~func_id = BackORC.reset_func_profile func_id
end :
  JIT_engine_lib.JITEngine.OnlineEngine)
