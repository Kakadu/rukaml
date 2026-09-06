# tests inferencer on built-in type { 'a list }

  $ run () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

assert that type of map is ('a -> 'b) -> 'a list -> 'b list
assert that type of fold is ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
assert that type of filter is ('a -> bool) -> 'a list -> 'a list
  $ run << EOF
  > let rec map f ls =
  >   match ls with
  >   | [] -> []
  >   | hd :: tl -> f hd :: map f tl
  > 
  > let rec fold f acc ls =
  >   match ls with
  >   | [] -> acc
  >   | hd :: tl -> fold f (f acc hd) tl
  > 
  > let rec filter pred ls =
  >   match ls with
  >   | [] -> []
  >   | hd :: tl ->
  >     let tl = filter pred tl in
  >     if pred hd then tl else hd :: tl
  > EOF
  let rec map: ('_3 -> '_4) -> '_3 list -> '_4 list =
    fun f ls -> match ls with
                  | [] -> []
                  | hd :: tl -> (f hd) :: ((map f) tl)
  let rec fold: ('_2 -> '_4 -> '_2) -> '_2 -> '_4 list -> '_2 =
    fun f acc ls -> match ls with
                      | [] -> acc
                      | hd :: tl -> ((fold f) ((f acc) hd)) tl
  let rec filter: ('_4 -> bool) -> '_4 list -> '_4 list =
    fun pred ls -> match ls with
                     | [] -> []
                     | hd :: tl -> let tl : '_4 list = (filter pred) tl in
                     (if pred hd then tl else hd :: tl)


assert that type of rev is 'a list -> 'a list
assert that type of join is 'a list -> 'b list -> ('a * 'b) list
assert that type of cat is 'a list -> 'a list -> 'a list
  $ run << EOF
  > let rev ls =
  >   let rec aux ls acc =
  >     match ls with
  >     | [] -> acc
  >     | hd :: tl -> aux tl (hd :: acc)
  >   in
  >   aux ls []
  > 
  > let rec join xs ys =
  >   match (xs, ys) with
  >   | [], _ -> []
  >   | _, [] -> []
  >   | xhd :: xtl, yhd :: ytl ->
  >     (xhd, yhd) :: join xtl ytl
  > 
  > let cat xs ys =
  >   let rec aux xs ys =
  >     match (xs, ys) with
  >     | [], acc -> acc
  >     | hd :: tl, acc -> aux tl (hd :: acc)
  >    in
  >  aux (rev xs) ys   
  > EOF
  let rec __lifted_let_1_aux: '_3 list -> '_3 list -> '_3 list =
    fun ls acc -> match ls with
                    | [] -> acc
                    | hd :: tl -> (__lifted_let_1_aux tl) (hd :: acc)
  let rev: '_4 list -> '_4 list =
    fun ls -> (__lifted_let_1_aux ls) []
  let rec join: '_3 list -> '_7 list -> ('_3 * '_7) list =
    fun xs ys -> match (xs, ys) with
                   | ([], _) -> []
                   | (_, []) -> []
                   | (xhd :: xtl, yhd :: ytl) -> (xhd, yhd) :: ((join xtl) ytl)
  let rec __lifted_let_2_aux: '_3 list -> '_3 list -> '_3 list =
    fun xs ys -> match (xs, ys) with
                   | ([], acc) -> acc
                   | (hd :: tl, acc) -> (__lifted_let_2_aux tl) (hd :: acc)
  let cat: '_5 list -> '_5 list -> '_5 list =
    fun xs ys -> (__lifted_let_2_aux (rev xs)) ys

assert that type of is_empty is 'a list -> bool
assert that type of exists is ('a -> bool) -> 'a list -> bool
assert that type of forall is ('a -> bool) -> 'a list -> bool
  $ run << EOF
  > let is_empty ls =
  >   match ls with
  >   | _ :: _ -> false
  >   | _ -> true
  > 
  > let rec exists pred ls =
  >   match ls with
  >   | [] -> false
  >   | hd :: tl -> if pred hd then true else exists pred tl
  > 
  > let rec forall pred ls =
  >   match ls with
  >   | [] -> true
  >   | hd :: tl -> if pred hd then forall pred tl else false
  > EOF
  let is_empty: '_3 list -> bool =
    fun ls -> match ls with
                | _ :: _ -> false
                | _ -> true
  let rec exists: ('_3 -> bool) -> '_3 list -> bool =
    fun pred ls -> match ls with
                     | [] -> false
                     | hd :: tl -> (if pred hd then true else (exists pred) tl)
  let rec forall: ('_3 -> bool) -> '_3 list -> bool =
    fun pred ls -> match ls with
                     | [] -> true
                     | hd :: tl -> (if pred hd then (forall pred) tl else false)


assert that type of len is 'a list -> int
  $ run << EOF
  > let rec len ls =
  >   match ls with
  >   | [] -> 0
  >   | _ :: xs -> 1 + len xs
  > EOF
  let rec len: '_2 list -> int =
    fun ls -> match ls with
                | [] -> 0
                | _ :: xs -> 1 + (len xs)


assert that type of equal is ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  $ run << EOF
  > let rec equal item_eq a b =
  >   match (a, b) with
  >   | [], [] -> true
  >   | x :: xs, y :: ys ->
  >     if item_eq x y then equal item_eq xs ys else false
  >   | _ -> false
  > EOF
  let rec equal: ('_4 -> '_5 -> bool) -> '_4 list -> '_5 list -> bool =
    fun item_eq a b -> match (a, b) with
                         | ([], []) -> true
                         | (x :: xs, y :: ys) -> (if (item_eq x) y then ((equal item_eq) xs) ys else false)
                         | _ -> false


assert that type of skip is int -> 'a list -> 'a list
  $ run << EOF
  > let rec skip n ls =
  >   match ls with
  >   | [] -> []
  >   | x :: xs ->
  >     let tail = skip (n - 1) xs in
  >       if n > 0 then x :: tail else tail
  > EOF
  let rec skip: int -> '_4 list -> '_4 list =
    fun n ls -> match ls with
                  | [] -> []
                  | x :: xs -> let tail : '_4 list = (skip (n - 1)) xs in
                  (if n > 0 then x :: tail else tail)


assert that type of take is int -> 'a list -> 'a list
  $ run << EOF
  > let rec take n ls =
  >   match ls with
  >   | [] -> []
  >   | x :: xs -> if n < 1 then [] else x :: take (n - 1) xs
  > EOF
  let rec take: int -> '_4 list -> '_4 list =
    fun n ls -> match ls with
                  | [] -> []
                  | x :: xs -> (if n < 1 then [] else x :: ((take (n - 1)) xs))
