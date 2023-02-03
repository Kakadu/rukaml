val set_logging : bool -> unit

module String_set : sig
  type elt = string

  include Stdlib.Set.S with type elt := string

  val pp : Format.formatter -> t -> unit
end

val free_vars_of_expr : Miniml.Parsetree.expr -> String_set.t
val standart_globals : String_set.t

(** Closure conversion. *)
val conv
  :  ?standart_globals:String_set.t
  -> Miniml.Parsetree.value_binding
  -> Miniml.Parsetree.value_binding list
