(** Abstract contract for a JIT backend.

    The driver only knows this module type; a concrete backend (ORC or
    any future one) implements it and is chosen through a type
    annotation in the driver:
    {[
      module Engine : JIT_engine_lib.JITEngine.BackEngine = JIT_orc_lib.OrcEngine
    ]}

    Error contract: every function may raise [Failure] with a
    backend-provided message — the driver prints it as ["jit error: ..."]. *)
module type BackEngine = sig
  (** Engine handle. For ORC it is a marker: state is process-global
      (a single [g_jit]), but the contract is instance-based so that
      future backends can hold real instances. *)
  type t

  (** Backend name ("orc") — for error messages and future backend
      selection. *)
  val name : string

  (** Create the engine and bring up the rukaml runtime. Idempotent
      within a process until [close] is called. *)
  val create : unit -> t

  (** Load a payload. Format [IR.TextLL] is required; [IR.Bitcode] →
      [Failure] (reserved, nothing produces it yet). Compilation may be
      lazy — the contract only requires that after [load] [run] sees the
      loaded functions. *)
  val load : t -> IR.t -> unit

  (** Look up a compiled function by name and call it: everything the
      LLVM backend compiles is [i64 (...args) -> i64], hence [int64].
      The program entry point is ["main"] (passed by the driver). *)
  val run : t -> string -> int64

  (** Shut the engine down (repeated calls are safe); [create] after
      [close] yields a fresh instance. *)
  val close : t -> unit

  val runtime_symbols : string list
end
