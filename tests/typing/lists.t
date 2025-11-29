# tests inferencer on built-in type { 'a list }

  $ run () { ../../driver/driver.exe $1 --target infer-parsetree -o a.ml && cat a.ml; }

# assert type of map is ('a -> 'b) -> 'a list -> 'b list
# assert type of fold is ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
# assert type of filter is ('a -> bool) -> 'a list -> 'a list
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
  let rec map: ('_3 -> '_4) -> '_3 list -> '_4 list =
    fun f ls -> match ls with
                  | [] -> []
                  | hd :: tl -> f hd :: ((map f) tl)
  let rec fold: ('_2 -> '_4 -> '_2) -> '_2 -> '_4 list -> '_2 =
    fun f acc ls -> match ls with
                      | [] -> acc
                      | hd :: tl -> ((fold f) ((f acc) hd)) tl
  let rec filter: ('_4 -> bool) -> '_4 list -> '_4 list =
    fun pred ls -> match ls with
                     | [] -> []
                     | hd :: tl -> let tl : '_4 list = (filter pred) tl in
                     (if pred hd then tl else hd :: tl)
#

# assert type of rev is 'a list -> 'a list
# assert type of join is 'a list -> 'b list -> ('a * 'b) list
# assert type of cat is 'a list -> 'a list -> 'a list
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
  let rev: '_13 list -> '_13 list =
    fun ls -> let rec aux : '_13 list -> '_13 list -> '_13 list = fun ls acc -> 
    match ls with
      | [] -> acc
      | hd :: tl -> (aux tl) hd :: acc in (aux ls) []
  let rec join: '_3 list -> '_7 list -> '_3 * '_7 list =
    fun xs ys -> match (xs, ys) with
                   | ([], _) -> []
                   | (xhd :: xtl, yhd :: ytl) -> (xhd, yhd) :: ((join xtl) ytl)
                   | (_, []) -> []
  let cat: '_15 list -> '_15 list -> '_15 list =
    fun xs ys -> let rec aux : '_15 list -> '_15 list -> '_15 list = fun xs ys -> 
    match (xs, ys) with
      | ([], acc) -> acc
      | (hd :: tl, acc) -> (aux tl) hd :: acc in (aux (rev xs)) ys

# assert type of is_empty is 'a list -> bool
# assert type of exists is ('a -> bool) -> 'a list -> bool
# assert type of forall is ('a -> bool) -> 'a list -> bool
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
#
