(** JIT backend contract. The driver knows only this module type; the ORC
    backend implements it. Every function may raise [Failure]. *)
module type BackEngine = sig
  (** Engine handle. ORC keeps its state in a process-global singleton. *)
  type t

  (** Backend name, e.g. ["orc"]. *)
  val name : string

  (** Creates the engine and brings up the runtime; idempotent until [close]. *)
  val create : unit -> t

  (** Loads a module. [IR.Bitcode] raises [Failure]; compilation may be lazy. *)
  val load : t -> IR.t -> unit

  (** Calls a compiled function by name; every function is [i64 -> i64]. *)
  val run : t -> string -> int64

  (** Shuts the engine down; safe to call repeatedly. *)
  val close : t -> unit

  val runtime_symbols : string list
end

type event =
  | Hot of int (** Function id became hot. *)
  | Timeout
  | Stop

(** Tiered-JIT extensions. *)
module type OnlineEngine = sig
  include BackEngine

  (** Loads a delta module under a fresh per-version resource tracker. *)
  val load_delta : t -> IR.t -> unit

  (** Raw address of a compiled symbol. *)
  val symbol_address : t -> string -> int64

  (** Binds the Tier 0 tables for [num_funcs] slotted functions. *)
  val bind_tables : t -> int -> unit

  (** Redirects [slot] to [symbol]. *)
  val install : t -> slot:int -> symbol:string -> unit

  (** Waits up to [timeout_ms] for the next event. *)
  val next_event : t -> timeout_ms:int -> event

  (** Wakes the event loop with [Stop]. *)
  val stop_events : t -> unit

  (** Constant parameter values ([None] when unseen or varying). *)
  val read_arg_profile : t -> func_id:int -> arity:int -> int64 option array

  (** Monomorphic closure target of a call site. *)
  val read_site_profile : t -> site_id:int -> (int64 * int * int) option

  (** [(cnt, miss)] counters of [func_id]. *)
  val read_counts : t -> func_id:int -> int64 * int64

  (** Resets the profile and the queued flag of [func_id]. *)
  val reset_func_profile : t -> func_id:int -> unit
end
