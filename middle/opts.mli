type opt_type = TailCall
type opt_state = 
  | OptEnabled 
  | OptLocallyDisabled (** for code regions, where opt is not allowed *)
  | OptGloballyDisabled (** when user doesn't pass "enable some optimization" to the compiler (e.g. -O2) *)

(** Settings of compilation, which represent enabled or disabled some optimization. *)
val opts : (opt_type, opt_state) Hashtbl.t

(** Check whether optimization is permitted now or not. *)
val is_permitted : opt_type -> bool

(** Set optimizations' config *)
val init_opts : ?configured_opts:(opt_type * opt_state) list -> unit -> unit

(** For disable optimization in local regions where that's not allowed *)
val disable_locally : opt_type -> (unit -> 'a) -> 'a

(* Now, only AMD64 backend uses this module.
   If we rewrite calling convention there like RV64,
   then we can remove this module
*)
module TailCall :
  (* Used in: AMD64 backend
     Goal of this module: find amount of additional space needed for function arguments
     Why this is needed: see in [/doc/tailcall.tex]

    How this works:
      We search all top-level functions which does tailcalls, collecting them in [tailcalls] hashtable.
      Each function has initial "amount of arguments" value, which stored in [funcs_stack_space] after invoking [build_tailcalls_table]
      After call of [init_ss_table], values in [funcs_stack_space] may be increased.
      These new values are final, which are needed for correct work of optimization.

    How values in [funcs_stack_space] are calculated:
      Let us have the following functions with their initial values.
      Then [tailcalls] table looks like:
      f(4) -> [g(3), h(5)]
      g(3) -> [k(8)]
      h(5) -> []
      After calcutaing, we have in [funcs_stack_space] (excluding rounding):
      f(8), g(8), k(8), h(5)
      Roughly speaking, we are simply looking for the maximum number of arguments among the chain of tail calls.
      Also this implementation has "visited" check for elimination of infinity recursion.
  *)
  
  sig
    (** Calculate tailcall optimization's necessary data *)
    val init_ss_table :
      [> `Function of Frontend.Ident.t * ANF.apat list * ANF.expr ] list ->
      unit
    
    (** Receive the total space occupied by function arguments *)
    val get_stack_space : Frontend.Ident.t -> int

    (** Receive the additional space occupied by function arguments *)
    val get_free_space : Frontend.Ident.t -> int -> int

    (** For compability with AMD64 *)
    val vb_to_function :
      ANF.stru_item ->
      [> `Function of Frontend.Ident.t * ANF.apat list * ANF.expr ] option
  end
