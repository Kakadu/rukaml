(** Low-level RukamlJIT bindings. External use goes through
    [JITEngine.BackEngine] (see [OrcEngine]). *)

external create : unit -> unit = "rukaml_orc_create"

(** Adds an [IR.TextLL] module; compilation happens on first lookup. *)
external update : string -> unit = "rukaml_orc_update"

(** Looks up and calls a symbol; every rukaml function is [i64 -> i64]. *)
external run : string -> int64 = "rukaml_orc_run"

(** Destroys the JIT; repeated calls are safe. *)
external destroy : unit -> unit = "rukaml_orc_destroy"

(** Runtime symbols registered as absolute by [create]. *)
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
