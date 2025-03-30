module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONAD_IDX1 = sig
  type ('i, 'a) t

  val return : 'a -> ('i, 'a) t
  val ( >>= ) : ('i, 'a) t -> ('a -> ('i, 'b) t) -> ('i, 'b) t
  val ( let* ) : ('i, 'a) t -> ('a -> ('i, 'b) t) -> ('i, 'b) t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Store : sig
  include MONAD_IDX1

  val get : ('acc, 'acc) t
  val put : 'acc -> ('acc, unit) t
  val ( <*> ) : 'acc 'a 'b. ('acc, 'a -> 'b) t -> ('acc, 'a) t -> ('acc, 'b) t
  val run : ('acc, 'a) t -> 'acc -> 'acc * 'a
end = struct
  type ('acc, 'a) t = 'acc -> 'acc * 'a

  let return : 'a 'acc. 'a -> ('acc, 'a) t = fun x acc -> acc, x
  let get : ('acc, 'acc) t = fun acc -> acc, acc
  let put : 'acc -> ('acc, unit) t = fun next _ -> next, ()
  let run m init = m init

  let ( <*> ) : 'acc 'a 'b. ('acc, 'a -> 'b) t -> ('acc, 'a) t -> ('acc, 'b) t =
    fun f arg acc ->
    let acc2, f = f acc in
    let acc3, arg = arg acc2 in
    acc3, f arg
  ;;

  let ( >>= ) : 'acc 'a 'b. ('acc, 'a) t -> ('a -> ('acc, 'b) t) -> ('acc, 'b) t =
    fun x f st ->
    let st, rez = x st in
    f rez st
  ;;

  let ( let* ) = ( >>= )
end

module Cont : sig
  include MONADERROR

  val run_cont : ('a -> 'b) -> 'a t -> 'b

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val list_mapm : ('a -> 'b t) -> 'a list -> 'b list t
end = struct
  type ('a, 'b) cont = 'a -> 'b
  type 'a t = { cont : 'b. ('a, 'b) cont -> 'b }

  let return (x : 'a) = { cont = (fun k -> k x) }

  let ( >>= ) (x : 'a t) (f : 'a -> 'b t) : 'b t =
    { cont = (fun k -> x.cont (fun v -> (f v).cont k)) }
  ;;

  let error = failwith
  let run_cont f { cont } = cont f

  module Syntax = struct
    let ( let* ) = ( >>= )
  end

  open Syntax

  let rec list_mapm f xs =
    match xs with
    | [] -> return []
    | h :: tl ->
      let* h = f h in
      let* tl = list_mapm f tl in
      return (h :: tl)
  ;;
end
