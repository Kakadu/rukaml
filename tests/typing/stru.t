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
  let rec zed: '_1 -> '_2 -> '_5 =
    fun f x -> (f (zed f)) x
  let fac: '_1 -> '_2 -> int =
    fun self n -> (if n = 1 then 1 else n * (self (n - 1)))
  let main: '_2 -> '_5 =
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
  let main: '_7 =
    (id idd) (id 1)

  $ run << EOF
  > let rec fix f = f (fix f)
  > EOF
  let rec fix: '_1 -> '_3 =
    fun f -> f (fix f)

  $ run << EOF
  > let rec fix f = f (fix f)
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = fix fac
  > EOF
  let rec fix: '_1 -> '_3 =
    fun f -> f (fix f)
  let fac: '_1 -> '_2 -> int =
    fun self n -> (if n = 1 then 1 else n * (self (n - 1)))
  let main: '_5 =
    fix fac

  $ run << EOF
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF
  let rec zed: '_1 -> '_2 -> '_5 =
    fun f x -> (f (zed f)) x
  let fac: '_1 -> '_2 -> int =
    fun self n -> (if n = 1 then 1 else n * (self (n - 1)))
  let main: '_2 -> '_5 =
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
  let rec fac: '_1 -> int =
    fun n -> (if n = 1 then 1 else n * (fac (n - 1)))
  let main: '_1 -> int =
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
  let add: '_1 -> '_2 -> int =
    fun x y -> x + y
  let add1: '_4 -> int =
    add 1
  let main: int =
    add1 13

  $ run << EOF
  > let add = fun x -> x + x
  > let add1 = add 1
  > let main = add 1
  > EOF
  let add: '_1 -> int =
    fun x -> x + x
  let add1: int =
    add 1
  let main: int =
    add 1

tuples
  $ run << EOF
  > let twice = fun x -> (x,x)
  > EOF
  Fatal error: exception Failure("unimplemented in elimination for product")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Frontend__Inferencer.elim in file "front/inferencer.ml", line 488, characters 9-20
  Called from Frontend__Inferencer.infer.helper in file "front/inferencer.ml", line 600, characters 23-44
  Called from Frontend__Inferencer.R.(>>=) in file "front/inferencer.ml", line 76, characters 18-22
  Called from Frontend__Inferencer.R.(>>=) in file "front/inferencer.ml", line 76, characters 18-22
  Called from Frontend__Inferencer.R.run in file "front/inferencer.ml" (inlined), line 108, characters 60-70
  Called from Frontend__Inferencer.vb in file "front/inferencer.ml", line 688, characters 2-10
  Called from Frontend__Inferencer.structure.(fun) in file "front/inferencer.ml", line 710, characters 29-41
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Frontend__Inferencer.structure in file "front/inferencer.ml", lines 705-711, characters 4-38
  Called from Dune__exe__Driver.Compiler.infer in file "driver/driver.ml", line 80, characters 10-35
  Called from Dune__exe__Driver in file "driver/driver.ml", line 218, characters 19-32
  [2]

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
  Fatal error: exception Failure("unimplemented in elimination for product")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Frontend__Inferencer.elim in file "front/inferencer.ml", line 488, characters 9-20
  Called from Frontend__Inferencer.infer.helper in file "front/inferencer.ml", line 600, characters 23-44
  Called from Frontend__Inferencer.R.(>>=) in file "front/inferencer.ml", line 76, characters 18-22
  Called from Frontend__Inferencer.R.(>>=) in file "front/inferencer.ml", line 76, characters 18-22
  Called from Frontend__Inferencer.R.run in file "front/inferencer.ml" (inlined), line 108, characters 60-70
  Called from Frontend__Inferencer.vb in file "front/inferencer.ml", line 688, characters 2-10
  Called from Frontend__Inferencer.structure.(fun) in file "front/inferencer.ml", line 710, characters 29-41
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Frontend__Inferencer.structure in file "front/inferencer.ml", lines 705-711, characters 4-38
  Called from Dune__exe__Driver.Compiler.infer in file "driver/driver.ml", line 80, characters 10-35
  Called from Dune__exe__Driver in file "driver/driver.ml", line 218, characters 19-32
  [2]

  $ run << EOF
  > let rec fac = fun n -> n*fac
  > EOF
  let rec fac: '_1 -> int =
    fun n -> n * fac

  $ run << EOF
  > let rec (a,b) = (a,b)
  > EOF
  infer error: Only variables are allowed as left-hand side of `let rec'
  [1]

  $ cat << EOF | ./REPL.exe -stru -
  > let f = (1, 2, true, 2)
  /tmp/dune_cram_ae226b_.cram.sh/main.sh: 1: /tmp/dune_cram_ae226b_.cram.sh/22.sh: ./REPL.exe: not found
  [127]

  $ cat << EOF | ./REPL.exe -stru -
  > let f = [||]
  /tmp/dune_cram_ae226b_.cram.sh/main.sh: 1: /tmp/dune_cram_ae226b_.cram.sh/23.sh: ./REPL.exe: not found
  [127]

  $ cat << EOF | ./REPL.exe -stru -
  > let f = [|1; 2; 3; 4|]
  /tmp/dune_cram_ae226b_.cram.sh/main.sh: 1: /tmp/dune_cram_ae226b_.cram.sh/24.sh: ./REPL.exe: not found
  [127]

  $ cat << EOF | ./REPL.exe -stru -
  > let f = [|1;true; 2; 3; 4|]
  /tmp/dune_cram_ae226b_.cram.sh/main.sh: 1: /tmp/dune_cram_ae226b_.cram.sh/25.sh: ./REPL.exe: not found
  [127]


  $ cat << EOF | ./REPL.exe -stru -
  > let f = [|[||]|]
  > let f = [|[||]|]
  /tmp/dune_cram_ae226b_.cram.sh/main.sh: 1: /tmp/dune_cram_ae226b_.cram.sh/26.sh: ./REPL.exe: not found
  [127]

  $ cat << EOF | ./REPL.exe -stru -
  > let f x y = x
  > let g = f 1
  > let a = g 2
  /tmp/dune_cram_ae226b_.cram.sh/main.sh: 1: /tmp/dune_cram_ae226b_.cram.sh/27.sh: ./REPL.exe: not found
  [127]

