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
  type t =
    | Foo of (int * int * int)
    | Bar of int * int * int
  let f: t -> int =
    fun x -> match x with
               | Bar (a, b, c) -> (a + b) + c
