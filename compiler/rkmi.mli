type item =
  { name : string
  ; arity : int
  ; typ : Miniml.Typedtree.scheme
  }

type info = item list

val output : string -> info -> unit
val from_file : string -> info

type repr

val to_repr : info list -> repr
val find_exn : string -> repr -> item
val length : repr -> int
