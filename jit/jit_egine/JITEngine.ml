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

type event =
  | Hot of int
  | Timeout
  | Stop

(** Extended contract for an online (tiered) JIT backend. *)
module type OnlineEngine = sig
  include BackEngine

  (** Load a delta IR module under a fresh per-version ResourceTracker. *)
  val load_delta : t -> IR.t -> unit

  (** Look up a compiled symbol's raw address. *)
  val symbol_address : t -> string -> int64

  (** Bind the Tier 0 global tables (@rukaml.slots, @rukaml.cnt, @rukaml.miss)
      for a module with [num_funcs] slotted functions. *)
  val bind_tables : t -> int -> unit

  (** Atomically redirect [@rukaml.slots[slot]] to the address of [symbol]. *)
  val install : t -> slot:int -> symbol:string -> unit

  (** Wait up to [timeout_ms] for the next hot-function event. *)
  val next_event : t -> timeout_ms:int -> event

  (** Signal the event queue to wake up and return [Stop]. *)
  val stop_events : t -> unit

  (** Read the sampled argument profile for [func_id] (length [arity]):
      [Some v] when the parameter was constant [v] across the sampling window,
      [None] when unseen or varied. *)
  val read_arg_profile : t -> func_id:int -> arity:int -> int64 option array

  (** Read the sampled closure target for [site_id]:
      [Some (code_addr, argsc, args_received)] if monomorphic. *)
  val read_site_profile : t -> site_id:int -> (int64 * int * int) option

  (** Read [(cnt, miss)] counters for [func_id]. *)
  val read_counts : t -> func_id:int -> int64 * int64

  (** Reset [cnt], [miss], argument profile, and queued flag for [func_id]. *)
  val reset_func_profile : t -> func_id:int -> unit
end
