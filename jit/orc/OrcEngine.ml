(** ORC implementation of the [JIT_engine_lib.JITEngine.BackEngine]
    contract.

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

  let run () symbol = BackORC.run symbol
  let close = BackORC.destroy
  let runtime_symbols = BackORC.runtime_syms ()
end :
  JIT_engine_lib.JITEngine.BackEngine)
