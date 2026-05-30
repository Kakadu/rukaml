# let rec polymorphism does not work

  $ run () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

# TODO (fix): exists should have type ('a -> bool) -> 'a list -> bool
  $ run << EOF
  > let rec exists pred ls =
  >   match ls with
  >   | [] -> false
  >   | hd :: tl -> if pred hd then true else exists pred tl
  > 
  > let not x = if x then false else true
  > let rec is_even x = if x = 0 then true else not (is_even (x - 1))
  > 
  > let main = exists is_even [ 1; 2; 3; 4; 5 ]
  > EOF
  let rec exists: (int -> bool) -> int list -> bool =
    fun pred ls -> match ls with
                     | [] -> false
                     | hd :: tl -> (if pred hd then true else (exists pred) tl)
  let not: bool -> bool =
    fun x -> (if x then false else true)
  let rec is_even: int -> bool =
    fun x -> (if x = 0 then true else not (is_even (x - 1)))
  let main: bool =
    (exists is_even) [ 1; 2; 3; 4; 5 ]
#

# TODO (fix): map should have type ('a -> 'b) -> 'a list -> 'b list
  $ run << EOF
  > let rec map f ls =
  >   match ls with
  >   | [] -> []
  >   | hd :: tl -> f hd :: map f tl
  > 
  > let not x = if x then false else true
  > 
  > let main = map not [ true; false; true; false ]
  > EOF
  let rec map: (bool -> bool) -> bool list -> bool list =
    fun f ls -> match ls with
                  | [] -> []
                  | hd :: tl -> (f hd) :: ((map f) tl)
  let not: bool -> bool =
    fun x -> (if x then false else true)
  let main: bool list =
    (map not) [ true; false; true; false ]
#

# TODO (fix): type of apply_n must be ('a -> 'a) -> 'a -> int -> 'a
  $ run << EOF
  > let rec apply_n f x n = if n = 0 then x else apply_n f (f x) (n - 1)
  > 
  > let not x = if x then false else true
  > 
  > let main = apply_n not true 5
  > EOF
  let rec apply_n: (bool -> bool) -> bool -> int -> bool =
    fun f x n -> (if n = 0 then x else ((apply_n f) (f x)) (n - 1))
  let not: bool -> bool =
    fun x -> (if x then false else true)
  let main: bool =
    ((apply_n not) true) 5
#

# TODO (fix): unification should not fail here
  $ run << EOF
  > let rec apply_n f x n = if n = 0 then x else apply_n f (f x) (n - 1)
  > 
  > let not x = if x then false else true
  > let inc x = x + 1
  > 
  > let main = (apply_n not true 2, apply_n inc 0 2)
  > EOF
  infer error: unification failed on bool and int
  [1]
#

# this one works as expected
  $ run << EOF
  > let apply f x = f x
  > 
  > let not x = if x then false else true
  > let inc x = x + 1
  > 
  > let main = (apply not true, apply inc 0)
  > EOF
  let apply: ('_2 -> '_3) -> '_2 -> '_3 =
    fun f x -> f x
  let not: bool -> bool =
    fun x -> (if x then false else true)
  let inc: int -> int =
    fun x -> x + 1
  let main: bool * int =
    (((apply not) true), ((apply inc) 0))
#
