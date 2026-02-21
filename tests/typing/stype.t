# tests inferencer on simple type declarations

  $ run () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

  $ run << EOF
  > type 'a box = | Box of 'a
  > 
  > let x = Box 1
  type '_0 box =
    | Box of '_0
  let x: int box =
    Box 1
  $ run << EOF
  > type ('a, 'b) result = 
  >   | Ok of 'a
  >   | Error of 'b
  > 
  > let x = Ok 1
  > let y = Error true
  type ('_0, '_1) result =
    | Ok of '_0
    | Error of '_1
  let x: (int, '_2) result =
    Ok 1
  let y: ('_1, bool) result =
    Error true

  $ run << EOF
  > type 'a list = 
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let x = Nil
  > let y = Cons (1, x)
  type '_0 list =
    | Cons of '_0 * '_0 list
    | Nil
  let x: '_1 list =
    Nil
  let y: int list =
    Cons (1, x)

  $ run << EOF
  > type ('a, 'b) arrows =
  >   | Normal of ('a -> 'b)
  >   | Reversed of ('b -> 'a)
  > 
  > let a = Normal (fun x -> x > 0)
  > let b = Reversed (fun x -> x > 0)
  type ('_0, '_1) arrows =
    | Normal of ('_0 -> '_1)
    | Reversed of ('_1 -> '_0)
  let fresh_1: ('_2 -> int -> '_4) -> '_2 -> '_4 =
    fun > x -> x > 0
  let a: (int, bool) arrows =
    Normal (fresh_1 >)
  let fresh_2: ('_2 -> int -> '_4) -> '_2 -> '_4 =
    fun > x -> x > 0
  let b: (bool, int) arrows =
    Reversed (fresh_2 >)
  $ run << EOF
  > type 'a pair =
  >   | Pair of 'a * 'a
  > 
  > let x = Pair ((1, 2), (3, 4))
  type '_0 pair =
    | Pair of '_0 * '_0
  let x: (int * int) pair =
    Pair ((1, 2), (3, 4))

  $ run << EOF
  > type 'a box =
  >   | Box of 'a
  > 
  > let x = Box (fun a -> a + 1)
  > 
  > let y = Box x
  type '_0 box =
    | Box of '_0
  let fresh_1: int -> int =
    fun a -> a + 1
  let x: (int -> int) box =
    Box fresh_1
  let y: (int -> int) box box =
    Box x

# assert type of { is_pair } is { 'a prod -> bool }
  $ run << EOF
  > type 'a prod =
  >   | Pair of 'a * 'a
  >   | Triple of 'a * 'a * 'a
  > 
  > let is_pair x =
  >   match x with
  >   | Pair (a, b) -> true
  >   | Triple (a, b, c) -> false
  type '_0 prod =
    | Pair of '_0 * '_0
    | Triple of '_0 * '_0 * '_0
  let is_pair: '_4 prod -> bool =
    fun x -> match x with
               | Pair (a, b) -> true
               | Triple (a, b, c) -> false
#

# assert type of { sum_prod } is { int prod -> int }
  $ run << EOF
  > type 'a prod =
  >   | Pair of 'a * 'a
  >   | Triple of 'a * 'a * 'a
  > 
  > let sum_prod x =
  >   match x with
  >   | Pair (a, b) -> a + b
  >   | Triple (a, b, c) -> a + b + c
  type '_0 prod =
    | Pair of '_0 * '_0
    | Triple of '_0 * '_0 * '_0
  let sum_prod: int prod -> int =
    fun x -> match x with
               | Pair (a, b) -> a + b
               | Triple (a, b, c) -> (a + b) + c
#

# unification should fail
  $ run << EOF
  > type 'a pair =
  >   | Pair of 'a * 'a
  > 
  > let x = Pair (1, true)
  infer error: unification failed on int and bool
  [1]
  $ run << EOF
  > type 'a pair =
  >   | Pair of 'a * 'a
  > 
  > let x = Pair (1, 2, 3)
  infer error: constructor arity mistmatch: Pair
  [1]

  $ run << EOF
  > type 'a list =
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let main =
  >   match Cons (true, Nil) with
  >   | Cons (x, y) -> x + 1
  infer error: unification failed on int and bool
  [1]

  $ run << EOF
  > type 'a box =
  >   | Box of 'a
  > 
  > let main =
  >   match (Box true) with
  >   | Box x -> x + 1
  infer error: unification failed on int and bool
  [1]

  $ run << EOF
  > type 'a pair =
  >   | Pair of 'a * 'a
  > 
  > let main =
  >   match Pair (1, 2) with
  >   | Pair (a, b, c) -> a + 1
  infer error: unification failed on ('_4, '_4) and ('_4, '_4, '_5)
  [1]
#

# tests inferencer on list primitives (list declarated with "type" instead of built-in list)

# assert type of { map } is { ('a -> 'b) -> 'a list -> 'b list }
# assert type of { fold } is { ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a }
# assert type of { filter } is { ('a -> bool) -> 'a list -> 'a list }
  $ run << EOF
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
  type '_0 list =
    | Cons of '_0 * '_0 list
    | Nil
  let rec map: ('_3 -> '_4) -> '_3 list -> '_4 list =
    fun f ls -> match ls with
                  | Nil -> Nil
                  | Cons (hd, tl) -> Cons (f hd, (map f) tl)
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
  $ run << EOF
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
  type '_0 list =
    | Cons of '_0 * '_0 list
    | Nil
  let rec aux: '_3 list -> '_3 list -> '_3 list =
    fun ls acc -> match ls with
                    | Nil -> acc
                    | Cons (hd, tl) -> (aux tl) (Cons (hd, acc))
  let rev: '_3 list -> '_3 list =
    fun ls -> (aux ls) Nil
  let is_empty: '_3 list -> bool =
    fun ls -> match ls with
                | Cons (_, _) -> false
                | Nil -> true

# assert type of { exists } is { ('a -> bool) -> 'a list -> bool }
# assert type of { forall } is { ('a -> bool) -> 'a list -> bool }
  $ run << EOF
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

# assert type of find is ('a -> bool) -> 'a list -> 'a option
  $ run << EOF
  > type 'a option =
  >   | Some of 'a
  >   | None
  > 
  > let rec find pred ls =
  >   match ls with
  >   | [] -> None
  >   | x :: xs -> if pred x then Some x else find pred xs
  type '_0 option =
    | Some of '_0
    | None
  let rec find: ('_4 -> bool) -> '_4 list -> '_4 option =
    fun pred ls -> match ls with
                     | [] -> None
                     | x :: xs -> (if pred x then Some x else (find pred) xs)
#

# assert type of find is ('a -> bool) -> 'a list -> 'a option
  $ run << EOF
  > type 'a option =
  >   | Some of 'a
  >   | None
  > 
  > let rec find pred ls =
  >   match ls with
  >   | [] -> None
  >   | x :: xs -> if pred x then Some x else find pred xs
  type '_0 option =
    | Some of '_0
    | None
  let rec find: ('_4 -> bool) -> '_4 list -> '_4 option =
    fun pred ls -> match ls with
                     | [] -> None
                     | x :: xs -> (if pred x then Some x else (find pred) xs)
#

# assert type of wrap is 'a list -> 'a option list
# assert type of unwrap is 'a option list -> 'a list option
  $ run << EOF
  > type 'a option =
  >   | Some of 'a
  >   | None
  > 
  > let rec wrap ls =
  >   match ls with
  >   | [] -> []
  >   | x :: xs -> Some x :: wrap ls
  > 
  > let unwrap ls =
  >   let rec aux ls acc =
  >     match ls with
  >     | [] -> Some acc
  >     | None :: _ -> None
  >     | Some x :: xs -> aux xs (x :: acc)
  >   in aux ls []
  >  
  type '_0 option =
    | Some of '_0
    | None
  let rec wrap: '_2 list -> '_2 option list =
    fun ls -> match ls with
                | [] -> []
                | x :: xs -> (Some x) :: (wrap ls)
  let rec aux: '_3 option list -> '_3 list -> '_3 list option =
    fun ls acc -> match ls with
                    | [] -> Some acc
                    | None :: _ -> None
                    | Some x :: xs -> (aux xs) (x :: acc)
  let unwrap: '_3 option list -> '_3 list option =
    fun ls -> (aux ls) []

#
