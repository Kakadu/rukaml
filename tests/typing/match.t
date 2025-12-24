  $ run () { ../../driver/driver.exe $1 --target typedtree --no-cconv -o a.ml && cat a.ml; }

  $ run << EOF
  > let main =
  >   match 1 with
  >   | x -> true
  let main: bool =
    match 1 with
      | x -> true

  $ run << EOF
  > let main =
  >   match (0, 1) with
  >   | (x, y) -> x + y
  let main: int =
    match (0, 1) with
      | (x, y) -> x + y

  $ run << EOF
  > let main =
  >   match (1, 2) with
  >   | _ -> (1, true)
  let main: int * bool =
    match (1, 2) with
      | _ -> (1, true)
  $ run << EOF
  > let main =
  >   match (5, true) with
  >   | (a, b) -> (a, b)
  let main: int * bool =
    match (5, true) with
      | (a, b) -> (a, b)
  $ run << EOF
  > let main =
  >   let f x = x + 1 in
  >   let x = 1 in
  >     match (f, x) with
  >     | (f, x) -> f x
  let main: int =
    let f : int -> int = fun x -> x + 1 in
    let x : int = 1 in
    match (f, x) with
      | (f, x) -> f x

  $ run << EOF
  > let main =
  >   let swap (x, y) = (y, x) in
  >   match swap (true, 1) with
  >   | (a, b) -> b
  let main: bool =
    let swap : '_1 * '_2 -> '_2 * '_1 = fun (x, y) -> (y, x) in
    match swap (true, 1) with
      | (a, b) -> b

  $ run << EOF
  > let main =
  >   let scnd (x, y) = y in
  >     match scnd (1, true) with
  >     | x -> x
  let main: bool =
    let scnd : '_1 * '_2 -> '_2 = fun (x, y) -> y in
    match scnd (1, true) with
      | x -> x
  $ run << EOF
  > let main x =
  >   match x with
  >   | (a, b) -> a + b
  let main: int * int -> int =
    fun x -> match x with
               | (a, b) -> a + b

  $ run << EOF
  > let main (x, y) =
  >   match (x, y) with
  >   | (a, b) -> a + b
  >   | x -> 1
  >   | _ -> 0
  let main: int * int -> int =
    fun (x, y) -> match (x, y) with
                    | (a, b) -> a + b
                    | x -> 1
                    | _ -> 0

  $ run << EOF
  > let main f x =
  >   match x with
  >   | (a, b) -> f a b
  let main: ('_3 -> '_4 -> '_6) -> '_3 * '_4 -> '_6 =
    fun f x -> match x with
                 | (a, b) -> (f a) b

  $ run << EOF
  > let first (x, y) =
  >   match (x, y) with
  >   | (a, b) -> a
  let first: '_1 * '_2 -> '_1 =
    fun (x, y) -> match (x, y) with
                    | (a, b) -> a

# unification should fail
  $ run << EOF
  > let main =
  >   match 1 with
  >   | x -> 1
  >   | _ -> true
  infer error: unification failed on bool and int
  [1]

  $ run << EOF
  > let main =
  >   match (1, 2) with
  >   | (a, b, c) -> a + b + c
  infer error: unification failed on (int, int, '_3) and (int, int)
  [1]
  $ run << EOF
  > let main =
  >   match (true, false) with
  >   | (a, b) -> a + b
  infer error: unification failed on int and bool
  [1]

  $ run << EOF
  > let main =
  >   match (true, false) with
  >   | (a, b) -> a + b
  infer error: unification failed on int and bool
  [1]

  $ run << EOF
  > let main =
  >     match 1 with
  >     | (a, b) -> a + b
  infer error: unification failed on ('_1, '_2) and int
  [1]

  $ run << EOF
  > let main =
  >   let x = 1 in
  >     match true with
  >     | z -> x + z
  infer error: unification failed on int and bool
  [1]

  $ run << EOF
  > let main =
  >   let f x = x + 1 in
  >     match (f, true) with
  >     | (g, y) -> g y
  infer error: unification failed on int and bool
  [1]

  $ run << EOF
  > let main =
  >   match true with
  >   | _ -> fun x -> x + 1
  >   | _ -> fun x -> fun y -> x + y
  infer error: unification failed on (int -> int) and int
  [1]

  $ run << EOF
  > let main =
  >     match (1, 2) with
  >     | (a, b) -> a + b
  >     | (a, b, c) -> a + b + c
  infer error: unification failed on (int, int, int) and (int, int)
  [1]
#
