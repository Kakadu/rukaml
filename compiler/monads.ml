module Store = struct
  type ('acc, 'a) t = 'acc -> 'acc * 'a

  let return : 'a 'acc. 'a -> ('acc, 'a) t = fun x acc -> acc, x
  let get : ('acc, 'a) t = fun acc -> acc, acc
  let put : 'acc -> ('acc, unit) t = fun next _ -> next, ()
  let run m init = m init

  let ( <*> ) : 'acc 'a 'b. ('acc, 'a -> 'b) t -> ('acc, 'a) t -> ('acc, 'b) t =
   fun f arg acc ->
    let acc2, f = f acc in
    let acc3, arg = arg acc2 in
    acc3, f arg
 ;;

  let ( let* ) : 'acc 'a 'b. ('acc, 'a) t -> ('a -> ('acc, 'b) t) -> ('acc, 'b) t =
   fun x f st ->
    let st, rez = x st in
    f rez st
 ;;
end
