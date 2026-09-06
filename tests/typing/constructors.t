  $ parse () { ../../driver/driver.exe $1 --target parsetree -o a.ml && cat a.ml; }
  $ infer () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

some weird edge-cases (which required fixing parser and typechecker)

adt constructors arity calculating is deferred from parsing stage to typechecking stage
although Foo is declared as Foo of (int * int), in the parsetree it is represented as Foo [ int; int ], whereas in the typedtree it is represented as Foo [ (int, int) ]

assert that int * int and (int * int) ARE NOT equivalent
  $ infer << EOF
  > type t =
  > | Foo of (int * int)
  > | Bar of int * int
  > EOF
  type t =
    | Foo of (int * int)
    | Bar of int * int

assert that int * int and (int * int) ARE equivalent
  $ infer << EOF
  > type t = int * int
  > type t = (int * int)
  > EOF
  type t = int * int
    
  type t = int * int
    
assert that Foo and Bar applications are syntactic equivalent
  $ infer << EOF
  > type t =
  > | Foo of (int * int * int)
  > | Bar of int * int * int
  > 
  > let t = Foo (1, 2, 3)
  > let t = Bar (1, 2, 3)
  > EOF
  type t =
    | Foo of (int * int * int)
    | Bar of int * int * int
  let t: t =
    Foo (1, 2, 3)
  let t: t =
    Bar (1, 2, 3)

should pass
  $ infer << EOF
  > type t =
  > | Foo of (int * int * int)
  > | Bar of int * int * int
  > 
  > let f x =
  >   match x with
  >   | Foo x -> x
  > EOF
  type t =
    | Foo of (int * int * int)
    | Bar of int * int * int
  let f: t -> int * int * int =
    fun x -> match x with
               | Foo x -> x

should pass
  $ infer << EOF
  > type t =
  > | Foo of (int * int * int)
  > | Bar of int * int * int
  > 
  > let f x =
  >   match x with
  >   | Foo (a, b, c) -> a + b + c
  > EOF
  type t =
    | Foo of (int * int * int)
    | Bar of int * int * int
  let f: t -> int =
    fun x -> match x with
               | Foo (a, b, c) -> (a + b) + c

should fail
  $ infer << EOF
  > type t =
  > | Foo of (int * int * int)
  > | Bar of int * int * int
  > 
  > let f x =
  >   match x with
  >   | Bar x -> x
  > EOF
  infer error: constructor arity mistmatch: Bar
  [1]


should pass
  $ infer << EOF
  > type t =
  > | Foo of (int * int * int)
  > | Bar of int * int * int
  > 
  > let f x =
  >   match x with
  >   | Bar (a, b, c) -> a + b + c
  > EOF
  type t =
    | Foo of (int * int * int)
    | Bar of int * int * int
  let f: t -> int =
    fun x -> match x with
               | Bar (a, b, c) -> (a + b) + c



  $ infer << EOF
  > type t = Foo of int * int
  > let x = Foo (1, 2)
  > EOF
  type t =
    | Foo of int * int
  let x: t =
    Foo (1, 2)


  $ infer << EOF
  > type t = Foo of (int * int)
  > let x = Foo (1, 2)
  > EOF
  type t =
    | Foo of (int * int)
  let x: t =
    Foo (1, 2)


should fail
  $ infer << EOF
  > type t = Foo of int * int
  > let x = 1, 2
  > let y = Foo x
  > EOF
  infer error: unification failed on int and (int * int)
  [1]

should pass
  $ infer << EOF
  > type t = Foo of (int * int)
  > let x = 1, 2
  > let y = Foo x
  > EOF
  type t =
    | Foo of (int * int)
  let x: int * int =
    (1, 2)
  let y: t =
    Foo x

  $ infer << EOF
  > type t = int
  > type foo = Foo of t
  > let x = Foo 1
  > EOF
  type t = int
    
  type foo =
    | Foo of int
  let x: foo =
    Foo 1


  $ infer << EOF
  > type ('a, 'b) pair = 'a * 'b
  > type foo = Foo of (int, string) pair
  > let x = Foo (1, "string")
  > EOF
  type ('_0, '_1) pair = '_0 * '_1
    
  type foo =
    | Foo of (int * string)
  let x: foo =
    Foo (1, "string")
