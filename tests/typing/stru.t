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

  let rec fac: '_1 -> int =
    fun n -> n * fac

  $ run << EOF
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF

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
  
  $ run << EOF
  > let rec fix f = f (fix f)
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = fix fac
  > EOF

  $ run << EOF
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF

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

    fun x -> x + x
  let add1: int =
    add 1
  let main: int =
    add 1

tuples
  $ run << EOF
  > let twice = fun x -> (x,x)
  > EOF


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


  $ run << EOF
  > let rec (a,b) = (a,b)
  > EOF
  infer error: Only variables are allowed as left-hand side of `let rec'
  [1]

  $ run << EOF
  > let f = (1, 2, true, 2)


  $ run << EOF
  > let f = [|1; 2; 3; 4|]
  /tmp/dune_cram_ae226b_.cram.sh/main.sh: 1: /tmp/dune_cram_ae226b_.cram.sh/24.sh: ./REPL.exe: not found
  [127]

  $ run << EOF
  > let f = [|1;true; 2; 3; 4|]
  /tmp/dune_cram_ae226b_.cram.sh/main.sh: 1: /tmp/dune_cram_ae226b_.cram.sh/25.sh: ./REPL.exe: not found
  [127]


  $ run << EOF
  > let f = [||]
  > let f = [|[||]|]


