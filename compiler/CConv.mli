val set_logging : bool -> unit

module String_set : sig
  include Stdlib.Set.S with type elt := string

  val pp : Format.formatter -> t -> unit
end

open Frontend

val free_vars_of_expr : Parsetree.expr -> String_set.t
val standart_globals : String_set.t

(** Closure conversion. *)

val conv
  :  ?standart_globals:String_set.t
  -> Parsetree.value_binding
  -> Parsetree.value_binding list

(** An alias for [conv] *)
val value_binding
  :  ?standart_globals:String_set.t
  -> Parsetree.value_binding
  -> Parsetree.value_binding list

val structure_item
  :  ?standart_globals:String_set.t
  -> Parsetree.structure_item
  -> Parsetree.structure

val structure
  :  ?standart_globals:String_set.t
  -> Parsetree.structure
  -> Parsetree.structure
