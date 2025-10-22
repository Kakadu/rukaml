  $ run () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

Zed combinator should trigger occurs check
  $ run << EOF
  > let main = fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v))
  > EOF
  infer error: Occurs check failed
  [1]

  $ run << EOF
  > let rec f = fun n -> f
  > EOF
  infer error: Occurs check failed
  [1]

  $ run << EOF
  > let rec fac = fun n -> n*fac
  > EOF
  infer error: unification failed on int and (int -> int)
  [1]


  $ run << EOF
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF
  let rec zed: ((int -> int) -> int -> int) -> int -> int =
    fun f x -> (f (zed f)) x
  let fac: (int -> int) -> int -> int =
    fun self n -> (if n = 1 then 1 else n * (self (n - 1)))
  let main: int -> int =
    zed fac

  $ run << EOF
  > let id = fun x -> x
  > let idd = fun x -> x
  > let main = (id idd) (id 1)
  > EOF
  let id: '_1 -> '_1 =
    fun x -> x
  let idd: '_1 -> '_1 =
    fun x -> x
  let main: int =
    (id idd) (id 1)

  $ run << EOF
  > let rec fix f = f (fix f)
  > EOF
  let rec fix: ('_3 -> '_3) -> '_3 =
    fun f -> f (fix f)

  $ run << EOF
  > let rec fix f = f (fix f)
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = fix fac
  > EOF
  let rec fix: ((int -> int) -> int -> int) -> int -> int =
    fun f -> f (fix f)
  let fac: (int -> int) -> int -> int =
    fun self n -> (if n = 1 then 1 else n * (self (n - 1)))
  let main: int -> int =
    fix fac

  $ run << EOF
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF
  let rec zed: ((int -> int) -> int -> int) -> int -> int =
    fun f x -> (f (zed f)) x
  let fac: (int -> int) -> int -> int =
    fun self n -> (if n = 1 then 1 else n * (self (n - 1)))
  let main: int -> int =
    zed fac

  $ run << EOF
  > (fun fix -> fun f -> f (fix f))
  > EOF
  parse error: : string
  [1]

  $ run << EOF
  > let rec s f g x = f x (g x) in s
  > EOF
  parse error: : end_of_input
  [1]

  $ run << EOF
  > let rec fac = fun n -> if n=1 then 1 else n * (fac (n-1))
  > let main = fac
  > EOF
  let rec fac: int -> int =
    fun n -> (if n = 1 then 1 else n * (fac (n - 1)))
  let main: int -> int =
    fac

  $ run << EOF
  > fun f -> fun x -> f (f x)
  > EOF
  parse error: : string
  [1]

  $ run << EOF
  > fun x -> let v = x in v
  > EOF
  parse error: : string
  [1]

  $ run << EOF
  > let add = fun x -> fun  y -> x + y
  > let add1 = add 1
  > let main = add1 13
  > EOF
  let add: int -> int -> int =
    fun x y -> x + y
  let add1: int -> int =
    add 1
  let main: int =
    add1 13


  $ run << EOF
  > let add = fun x -> x + x
  > let add1 = add 1
  > let main = add 1
  > EOF
  let add: int -> int =
    fun x -> x + x
  let add1: int =
    add 1
  let main: int =
    add 1

tuples
  $ run << EOF
  > let twice = fun x -> (x,x)
  > EOF
  let twice: '_1 -> '_1 * '_1 =
    fun x -> (x, x)


  $ run << EOF
  > let foo x =
  >   let y = fun z -> z in
  >   y
  > EOF
  let y: '_1 -> '_1 =
    fun z -> z
  let foo: '_1 -> '_2 -> '_2 =
    fun x -> y

  $ run << EOF
  > let foo x =
  >   let y = fun z -> z in
  >   (y 1, y true)
  > EOF
  let y: '_1 -> '_1 =
    fun z -> z
  let foo: '_1 -> int * bool =
    fun x -> ((y 1), (y true))


  $ run << EOF
  > let rec (a,b) = (a,b)
  > EOF
  infer error: Only variables are allowed as left-hand side of `let rec'
  [1]

  $ run << EOF
  > let f = (1, 2, true, 2)
  let f: int * int * bool * int =
    (1, 2, true, 2)

  $ run << EOF
  > let f = [||]
  Fatal error: exception Failure("unimplemented in conv for arrays")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from CConv.conv.(fun) in file "middle/CConv.ml", line 306, characters 43-72
  Called from Dune__exe__Driver.Compiler.cconv.f in file "driver/driver.ml", line 69, characters 17-56
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Dune__exe__Driver.Compiler.cconv.(fun) in file "driver/driver.ml", line 74, characters 20-77
  Called from Dune__exe__Driver.Target.Intermediate.typedtree in file "driver/driver.ml", line 147, characters 28-55
  Called from Dune__exe__Driver in file "driver/driver.ml", line 218, characters 19-32
  [2]

  $ run << EOF
  > let f = [|1;true; 2; 3; 4|]
  Fatal error: exception Failure("unimplemented in conv for arrays")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from CConv.conv.(fun) in file "middle/CConv.ml", line 306, characters 43-72
  Called from Dune__exe__Driver.Compiler.cconv.f in file "driver/driver.ml", line 69, characters 17-56
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Dune__exe__Driver.Compiler.cconv.(fun) in file "driver/driver.ml", line 74, characters 20-77
  Called from Dune__exe__Driver.Target.Intermediate.typedtree in file "driver/driver.ml", line 147, characters 28-55
  Called from Dune__exe__Driver in file "driver/driver.ml", line 218, characters 19-32
  [2]

  $ run << EOF
  > let f = [||]
  > let f = [|[||]|]
  Fatal error: exception Failure("unimplemented in conv for arrays")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from CConv.conv.(fun) in file "middle/CConv.ml", line 306, characters 43-72
  Called from Dune__exe__Driver.Compiler.cconv.f in file "driver/driver.ml", line 69, characters 17-56
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Dune__exe__Driver.Compiler.cconv.(fun) in file "driver/driver.ml", line 74, characters 20-77
  Called from Dune__exe__Driver.Target.Intermediate.typedtree in file "driver/driver.ml", line 147, characters 28-55
  Called from Dune__exe__Driver in file "driver/driver.ml", line 218, characters 19-32
  [2]

  $ run << EOF
  > let pair x = (x, x)
  > let g = [| pair |]
  Fatal error: exception Failure("unimplemented in conv for arrays")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from CConv.conv.(fun) in file "middle/CConv.ml", line 306, characters 43-72
  Called from Dune__exe__Driver.Compiler.cconv.f in file "driver/driver.ml", line 69, characters 17-56
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Dune__exe__Driver.Compiler.cconv.(fun) in file "driver/driver.ml", line 74, characters 20-77
  Called from Dune__exe__Driver.Target.Intermediate.typedtree in file "driver/driver.ml", line 147, characters 28-55
  Called from Dune__exe__Driver in file "driver/driver.ml", line 218, characters 19-32
  [2]

  $ run << EOF
  > let f x = [| x |]
  Fatal error: exception Failure("unimplemented in conv for arrays")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from CConv.conv.(fun) in file "middle/CConv.ml", line 306, characters 43-72
  Called from Dune__exe__Driver.Compiler.cconv.f in file "driver/driver.ml", line 69, characters 17-56
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Dune__exe__Driver.Compiler.cconv.(fun) in file "driver/driver.ml", line 74, characters 20-77
  Called from Dune__exe__Driver.Target.Intermediate.typedtree in file "driver/driver.ml", line 147, characters 28-55
  Called from Dune__exe__Driver in file "driver/driver.ml", line 218, characters 19-32
  [2]
Without eta-expansion
  $ run << EOF
  > let f x y = x
  > let g = f 1
  > let h = g 2
  > let h = g
  let f: '_1 -> '_2 -> '_1 =
    fun x y -> x
  let g: '_weak1 -> int =
    f 1
  let h: int =
    g 2
  let h: int -> int =
    g

With eta-expansion but we not use the passed argument
  $ run << EOF
  > let pair x y = (x, y)
  > let g x = pair 1
  > let temp = g 2
  let pair: '_1 -> '_2 -> '_1 * '_2 =
    fun x y -> (x, y)
  let g: '_1 -> '_4 -> int * '_4 =
    fun x -> pair 1
  let temp: '_weak1 -> int * '_weak1 =
    g 2

With eta-expansion
  $ run << EOF
  > let pair x y = (x, y)
  > let g x = pair 1 x
  let pair: '_1 -> '_2 -> '_1 * '_2 =
    fun x y -> (x, y)
  let g: '_1 -> int * '_1 =
    fun x -> (pair 1) x

  $ run << EOF
  > let f = [|1; 2; 3; 4|]
  Parsed.
  let f = [|1; 2; 3; 4|]
  let f: int array =
    [|1; 2; 3; 4|]

  $ run << EOF
  > let f = [|1;true; 2; 3; 4|]
  Parsed.
  let f = [|1; true; 2; 3; 4|]
  Error: unification failed on int and bool
