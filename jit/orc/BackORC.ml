(* Low-level bindings for RukamlJIT. Do not use them outside jit/orc:
   the driver-facing contract is JITEngine.BackEngine (see OrcEngine). *)

(* Idempotent within a process: creates [g_jit] (if there is none yet) and
   registers the rukaml runtime (back/llvm/rukaml_stdlib.c, compiled into
   this same .so — jit/orc/dune) as absolute symbols: no -rdynamic and no
   process-symbol generator in the JITDylib. Registration happens exactly
   once, under "just created", otherwise a second create would be a
   duplicate symbol definition. *)
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
