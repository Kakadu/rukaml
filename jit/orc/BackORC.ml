(* Low-level bindings for RukamlJIT. Do not use them outside jit/orc:
   the driver-facing contract is JITEngine.BackEngine (see OrcEngine). *)

external create : unit -> unit = "rukaml_orc_create"

(* Adds a textual LLVM IR ([IR.TextLL] payload) as a new module to the
   JITDylib; compilation is lazy — on the first lookup. *)
external update : string -> unit = "rukaml_orc_update"

(* Looks up a compiled symbol and calls it. Every rukaml function is
   compiled to i64 (...args) -> i64, hence int64; entry point — "main". *)
external run : string -> int64 = "rukaml_orc_run"

(* Destroys the JIT (endSession); repeated calls are safe, the next
   [create] makes a fresh instance. *)
external destroy : unit -> unit = "rukaml_orc_destroy"

(* Names of the runtime symbols registered as absolute symbols by
   [create]; read straight from the registration table, so it works with
   or without a JIT instance. *)
external runtime_syms : unit -> string list = "rukaml_orc_runtime_syms"
external add_delta : string -> unit = "rukaml_orc_add_delta"
external lookup_address : string -> int64 = "rukaml_orc_lookup_address"
external bind_tables : int -> unit = "rukaml_orc_bind_tables"
external install_slot : int -> string -> unit = "rukaml_orc_install_slot"
external next_event : int -> int = "rukaml_orc_next_event"
external stop_events : unit -> unit = "rukaml_orc_stop_events"

external read_arg_profile
  :  int
  -> int
  -> int64 option array
  = "rukaml_orc_read_arg_profile"

external read_site_profile
  :  int
  -> (int64 * int * int) option
  = "rukaml_orc_read_site_profile"

external read_counts : int -> int64 * int64 = "rukaml_orc_read_counts"
external reset_func_profile : int -> unit = "rukaml_orc_reset_func_profile"
