asserts that let polymorphism works as expected

  $ run () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

  $ run << EOF
  > let id x = x
  > let n = id 5
  > let b = id true
  > EOF
  let id: '_1 -> '_1 =
    fun x -> x
  let n: int =
    id 5
  let b: bool =
    id true

  $ run << EOF
  > let swap (x, y) = (y, x)
  > 
  > let (true, 1) = swap (1, true)
  > let (1, true) = swap (true, 1)
  > EOF
  let swap: '_1 * '_2 -> '_2 * '_1 =
    fun (x, y) -> (y, x)
  let (true, 1): bool * int =
    swap (1, true)
  let (1, true): int * bool =
    swap (true, 1)

  $ run << EOF
  > let apply f x = f x
  > 
  > let inc = apply (fun x -> x + 1)
  > let not = apply (fun x -> if x then false else true)
  > 
  > let 1 = inc 0
  > let true = not false
  > EOF
  let apply: ('_2 -> '_3) -> '_2 -> '_3 =
    fun f x -> f x
  let __lifted_lam_1: int -> int =
    fun x -> x + 1
  let inc: int -> int =
    apply __lifted_lam_1
  let __lifted_lam_2: bool -> bool =
    fun x -> (if x then false else true)
  let not: bool -> bool =
    apply __lifted_lam_2
  let 1: int =
    inc 0
  let true: bool =
    not false

  $ run << EOF
  > let compose f g x = g (f x)
  > 
  > let mul2 n = n * 2
  > let add3 n = n + 3
  > 
  > let not b = if b then false else true
  > 
  > let 6 = compose add3 mul2 0
  > let false = compose not not true
  > EOF
  let compose: ('_3 -> '_4) -> ('_4 -> '_5) -> '_3 -> '_5 =
    fun f g x -> g (f x)
  let mul2: int -> int =
    fun n -> n * 2
  let add3: int -> int =
    fun n -> n + 3
  let not: bool -> bool =
    fun b -> (if b then false else true)
  let 6: int =
    ((compose add3) mul2) 0
  let false: bool =
    ((compose not) not) true

  $ run << EOF
  > let twice f x = f (f x)
  > let inc x = x + 1
  > let not b = if b then false else true
  > let 2 = twice inc 0
  > let true = twice not false
  > EOF
  let twice: ('_4 -> '_4) -> '_4 -> '_4 =
    fun f x -> f (f x)
  let inc: int -> int =
    fun x -> x + 1
  let not: bool -> bool =
    fun b -> (if b then false else true)
  let 2: int =
    (twice inc) 0
  let true: bool =
    (twice not) false

  $ run << EOF
  > let is_empty ls =
  >   match ls with
  >   | [] -> true
  >   | _ -> false
  > 
  > let true = is_empty []
  > let false = is_empty [ true; false; true ]
  > let false = is_empty [ 1; 2; 3; 4 ]
  > EOF
  let is_empty: '_2 list -> bool =
    fun ls -> match ls with
                | [] -> true
                | _ -> false
  let true: bool =
    is_empty []
  let false: bool =
    is_empty [ true; false; true ]
  let false: bool =
    is_empty [ 1; 2; 3; 4 ]

  $ run << EOF
  > type 'a option =
  >   | None
  >   | Some of 'a
  > 
  > let list_head_opt ls =
  >   match ls with
  >   | [] -> None
  >   | x :: _ -> Some x
  > 
  > let None = list_head_opt []
  > let Some 1 = list_head_opt [ 1; 2; 3 ]
  > let Some true = list_head_opt [ true ]
  > EOF
  type '_0 option =
    | None
    | Some of '_0
  let list_head_opt: '_3 list -> '_3 option =
    fun ls -> match ls with
                | [] -> None
                | x :: _ -> Some x
  let None: '_weak1 option =
    list_head_opt []
  let Some 1: int option =
    list_head_opt [ 1; 2; 3 ]
  let Some true: bool option =
    list_head_opt [ true ]

stru let rec
  $ run << EOF
  > let rec apply_n f x n = if n < 1 then x else f (apply_n f x (n - 1))
  > 
  > let 5 = apply_n (fun x -> x + 1) 0 5
  > let false = apply_n (fun x -> if x then false else true) true 3
  > EOF
  let rec apply_n: ('_11 -> '_11) -> '_11 -> int -> '_11 =
    fun f x n -> (if n < 1 then x else f (((apply_n f) x) (n - 1)))
  let __lifted_lam_1: int -> int =
    fun x -> x + 1
  let 5: int =
    ((apply_n __lifted_lam_1) 0) 5
  let __lifted_lam_2: bool -> bool =
    fun x -> (if x then false else true)
  let false: bool =
    ((apply_n __lifted_lam_2) true) 3

  $ run << EOF
  > let rec exists pred ls =
  >   match ls with
  >   | [] -> false
  >   | hd :: tl -> if pred hd then true else exists pred tl
  >  
  > let true = exists (fun x -> x) [ false; false; true; false ]
  > let false = exists (fun x -> x < 0) [ 1; 2; 3; 4; 5 ]
  > EOF
  let rec exists: ('_3 -> bool) -> '_3 list -> bool =
    fun pred ls -> match ls with
                     | [] -> false
                     | hd :: tl -> (if pred hd then true else (exists pred) tl)
  let __lifted_lam_1: '_1 -> '_1 =
    fun x -> x
  let true: bool =
    (exists __lifted_lam_1) [ false; false; true; false ]
  let __lifted_lam_2: int -> bool =
    fun x -> x < 0
  let false: bool =
    (exists __lifted_lam_2) [ 1; 2; 3; 4; 5 ]

  $ run << EOF
  > let rec map f ls =
  >   match ls with
  >   | [] -> []
  >   | hd :: tl -> f hd :: map f tl
  > 
  > let bool_list = map (fun x -> x > 0) [ 1; 2; 3; 4; 5 ]
  > let int_list = map (fun x -> x + 1) [ 1; 2; 3; 4; 5 ]
  > EOF
  let rec map: ('_3 -> '_4) -> '_3 list -> '_4 list =
    fun f ls -> match ls with
                  | [] -> []
                  | hd :: tl -> (f hd) :: ((map f) tl)
  let __lifted_lam_1: int -> bool =
    fun x -> x > 0
  let bool_list: bool list =
    (map __lifted_lam_1) [ 1; 2; 3; 4; 5 ]
  let __lifted_lam_2: int -> int =
    fun x -> x + 1
  let int_list: int list =
    (map __lifted_lam_2) [ 1; 2; 3; 4; 5 ]

  $ run << EOF
  > let rec fix f = f (fix f)
  > EOF
  let rec fix: ('_3 -> '_3) -> '_3 =
    fun f -> f (fix f)

constants matching
  $ run << EOF
  > let (x, y) = (1, true)
  > let (x, true) = (1, true)
  > let (1, y) = (1, true)
  > let (1, true) = (1, true)
  > EOF
  let (x, y): int * bool =
    (1, true)
  let (x, true): int * bool =
    (1, true)
  let (1, y): int * bool =
    (1, true)
  let (1, true): int * bool =
    (1, true)

  $ run << EOF
  > let (x, _) = (1, true)
  > let (_, y) = (1, true)
  > let _ = (1, true)
  > EOF
  let (x, _): int * bool =
    (1, true)
  let (_, y): int * bool =
    (1, true)
  let _: int * bool =
    (1, true)

  $ run << EOF
  > let first (x, _) = x
  > let 1 = first (1, 2)
  > let true = first (true, false)
  > EOF
  let first: '_1 * '_2 -> '_1 =
    fun (x, _) -> x
  let 1: int =
    first (1, 2)
  let true: bool =
    first (true, false)

  $ run << EOF
  > let (id, inc) = (fun x -> x), (fun x -> x + 1)
  > EOF
  let __lifted_lam_1: '_1 -> '_1 =
    fun x -> x
  let __lifted_lam_2: int -> int =
    fun x -> x + 1
  let (id, inc): ('_2 -> '_2) * (int -> int) =
    (__lifted_lam_1, __lifted_lam_2)

invalid inputs

  $ run << EOF
  > let rec _ x = x + 1
  > EOF
  infer error: Only variables are allowed as left-hand side of `let rec'
  [1]

  $ run << EOF
  > let rec (f, g) x = (f x, g x)
  > EOF
  infer error: Only variables are allowed as left-hand side of `let rec'
  [1]

  $ run << EOF
  > let rec (a,b) = (a,b)
  > EOF
  infer error: Only variables are allowed as left-hand side of `let rec'
  [1]

  $ run << EOF
  > let rec f = fun x -> x f
  > EOF
  infer error: Occurs check failed
  [1]
