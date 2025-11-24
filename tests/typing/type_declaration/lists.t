# tests inferencer on list primitives 

# assert type of { map } is { ('a -> 'b) -> 'a list -> 'b list }
# assert type of { fold } is { ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a }
# assert type of { filter } is { ('a -> bool) -> 'a list -> 'a list }
  $ cat << EOF | ./run.exe
  > type 'a list =
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let rec map f ls =
  >   match ls with
  >   | Nil -> Nil
  >   | Cons (hd, tl) -> Cons (f hd, map f tl)
  > 
  > let rec fold f acc ls =
  >   match ls with
  >   | Nil -> acc
  >   | Cons (hd, tl) -> fold f (f acc hd) tl
  > 
  > let rec filter pred ls =
  >   match ls with
  >   | Nil -> Nil
  >   | Cons (hd, tl) ->
  >     let tl = filter pred tl in
  >     if pred hd then tl else Cons (hd, tl)
  result:
  type '_0 list =
    | Cons of '_0 * '_0 list
    | Nil
  let rec map: ('_3 -> '_4) -> '_3 list -> '_4 list =
    fun f ls -> match ls with
                  | Nil -> Nil
                  | Cons (hd, tl) -> Cons ((f hd), ((map f) tl))
  let rec fold: ('_2 -> '_4 -> '_2) -> '_2 -> '_4 list -> '_2 =
    fun f acc ls -> match ls with
                      | Nil -> acc
                      | Cons (hd, tl) -> ((fold f) ((f acc) hd)) tl
  let rec filter: ('_4 -> bool) -> '_4 list -> '_4 list =
    fun pred ls -> match ls with
                     | Nil -> Nil
                     | Cons (hd, tl) -> let tl : '_4 list = (filter pred) tl in
                     (if pred hd then tl else Cons (hd, tl))
  
#

# assert type of { rev } is { 'a list -> 'a list }
# assert type of { is_empty } is { 'a list -> bool }
  $ cat << EOF | ./run.exe
  > type 'a list =
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let rev ls =
  >   let rec aux ls acc =
  >     match ls with
  >     | Nil -> acc
  >     | Cons (hd, tl) -> aux tl (Cons (hd, acc))
  >   in
  >   aux ls Nil
  > 
  > let is_empty ls =
  >   match ls with
  >   | Cons (_, _) -> false
  >   | Nil -> true
  result:
  type '_0 list =
    | Cons of '_0 * '_0 list
    | Nil
  let rev: '_13 list -> '_13 list =
    fun ls -> let rec aux : '_13 list -> '_13 list -> '_13 list = fun ls acc -> 
    match ls with
      | Nil -> acc
      | Cons (hd, tl) -> (aux tl) (Cons (hd, acc)) in (aux ls) Nil
  let is_empty: '_3 list -> bool =
    fun ls -> match ls with
                | Cons (_, _) -> false
                | Nil -> true
  

# assert type of { exists } is { ('a -> bool) -> 'a list -> bool }
# assert type of { forall } is { ('a -> bool) -> 'a list -> bool }
  $ cat << EOF | ./run.exe
  > type 'a list =
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let rec exists pred ls =
  >   match ls with
  >   | Nil -> false
  >   | Cons (hd, tl) -> if pred hd then true else exists pred tl
  > 
  > let rec forall pred ls =
  >   match ls with
  >   | Nil -> true
  >   | Cons (hd, tl) -> if pred hd then forall pred tl else false
  result:
  type '_0 list =
    | Cons of '_0 * '_0 list
    | Nil
  let rec exists: ('_3 -> bool) -> '_3 list -> bool =
    fun pred ls -> match ls with
                     | Nil -> false
                     | Cons (hd, tl) -> (if pred hd then true else (exists pred) tl)
  let rec forall: ('_3 -> bool) -> '_3 list -> bool =
    fun pred ls -> match ls with
                     | Nil -> true
                     | Cons (hd, tl) -> (if pred hd then (forall pred) tl else false)
  
#
