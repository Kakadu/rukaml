Zed combinator should trigger occurs check
  $ cat << EOF | ./run.exe -stru -
  > let main = fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v))
  > EOF
  Error: Occurs check failed

  $ cat << EOF | ./run.exe -stru -
  > let rec f = fun n -> f
  > EOF
  Error: Occurs check failed

  $ cat << EOF | ./run.exe -stru -
  > let rec fac = fun n -> n*fac
  > EOF
  Error: unification failed on int and (int -> int)

  $ cat << EOF | ./run.exe -stru -
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

  $ cat << EOF | ./run.exe  -stru -
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

  $ cat << EOF | ./run.exe  -stru -
  > let rec fix f = f (fix f)
  > EOF
  let rec fix: ('_3 -> '_3) -> '_3 =
    fun f -> f (fix f)

  $ cat << EOF | ./run.exe -stru  -
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

  $ cat << EOF | ./run.exe  -stru -
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

  $ cat << EOF | ./run.exe  -stru -
  > (fun fix -> fun f -> f (fix f))
  > EOF
  Error: : end_of_input

  $ cat << EOF | ./run.exe  -stru -
  > let rec s f g x = f x (g x) in s
  > EOF
  Error: : end_of_input

  $ cat << EOF | ./run.exe  -stru -
  > let rec fac = fun n -> if n=1 then 1 else n * (fac (n-1))
  > let main = fac
  > EOF
  let rec fac: int -> int =
    fun n -> (if n = 1 then 1 else n * (fac (n - 1)))
  let main: int -> int =
    fac

  $ cat << EOF | ./run.exe  -stru -
  > fun f -> fun x -> f (f x)
  > EOF
  Error: : end_of_input

  $ cat << EOF | ./run.exe  -stru -
  > fun x -> let v = x in v
  > EOF
  Error: : end_of_input

  $ cat << EOF | ./run.exe  -stru -
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

  $ cat << EOF | ./run.exe  -stru -
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
  $ cat << EOF | ./run.exe  -stru -
  > let twice = fun x -> (x,x)
  > EOF
  let twice: '_1 -> '_1 * '_1 =
    fun x -> (x, x)

  $ cat << EOF | ./run.exe -stru -
  > let foo x =
  >   let y = fun z -> z in
  >   y
  > EOF
  let foo: '_1 -> '_3 -> '_3 =
    fun x -> let y : '_2 -> '_2 = fun z -> z in
    y

  $ cat << EOF | ./run.exe -stru -
  > let foo x =
  >   let y = fun z -> z in
  >   (y 1, y true)
  > EOF
  let foo: '_1 -> int * bool =
    fun x -> let y : '_2 -> '_2 = fun z -> z in
    ((y 1), (y true))

  $ cat << EOF | ./run.exe -stru -
  > let rec fac = fun n -> n*fac
  > EOF
  Error: unification failed on int and (int -> int)

  $ cat << EOF | ./run.exe -stru -
  > let rec (a,b) = (a,b)
  > EOF
  Error: Only variables are allowed as left-hand side of `let rec'
