
  $ cat << EOF | ./REPL.exe -stru -
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF
  "let rec zed f x = f (zed f) x\nlet fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))\nlet main = zed fac"
  Parsed.
  let rec zed: (((int -> int) -> (int -> int)) -> (int -> int)) =fun f -> fun x -> ((f (zed f)) x)
  let fac: ((int -> int) -> (int -> int)) =fun self -> fun n -> if (n = 1) then 1 else (n * (self (n - 1)))
  let main: (int -> int) =(zed fac) 

  $ cat << EOF | ./REPL.exe  -stru -
  > let id = fun x -> x
  > let idd = fun x -> x
  > let main = (id idd) (id 1)
  > EOF
  "let id = fun x -> x\nlet idd = fun x -> x\nlet main = (id idd) (id 1)"
  Parsed.
  let id: ('_1 -> '_1) =fun x -> x
  let idd: ('_1 -> '_1) =fun x -> x
  let main: int =((id idd) (id 1)) 
  $ cat << EOF | ./REPL.exe  -stru -
  > let rec fix f = f (fix f)
  > EOF
  "let rec fix f = f (fix f)"
  Parsed.
  let rec fix: (('_2 -> '_3) -> '_3) =fun f -> (f (fix f)) 

  $ cat << EOF | ./REPL.exe -stru  -
  > let rec fix f = f (fix f)
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = fix fac
  > EOF
  "let rec fix f = f (fix f)\nlet fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))\nlet main = fix fac"
  Parsed.
  let rec fix: (((int -> int) -> (int -> int)) -> (int -> int)) =fun f -> (f (fix f))
  let fac: ((int -> int) -> (int -> int)) =fun self -> fun n -> if (n = 1) then 1 else (n * (self (n - 1)))
  let main: (int -> int) =(fix fac) 

  $ cat << EOF | ./REPL.exe  -stru -
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF
  "let rec zed f x = f (zed f) x\nlet fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))\nlet main = zed fac"
  Parsed.
  let rec zed: (((int -> int) -> (int -> int)) -> (int -> int)) =fun f -> fun x -> ((f (zed f)) x)
  let fac: ((int -> int) -> (int -> int)) =fun self -> fun n -> if (n = 1) then 1 else (n * (self (n - 1)))
  let main: (int -> int) =(zed fac) 

  $ cat << EOF | ./REPL.exe  -stru -
  > (fun fix -> fun f -> f (fix f))
  > EOF
  "(fun fix -> fun f -> f (fix f))"
  Error: : end_of_input
  $ cat << EOF | ./REPL.exe  -stru -
  > let rec s f g x = f x (g x) in s
  > EOF
  "let rec s f g x = f x (g x) in s"
  Error: : end_of_input
  $ cat << EOF | ./REPL.exe  -stru -
  > let rec fac = fun n -> if n=1 then 1 else n * (fac (n-1))
  > let main = fac
  > EOF
  "let rec fac = fun n -> if n=1 then 1 else n * (fac (n-1))\nlet main = fac"
  Parsed.
  let rec fac: (int -> int) =fun n -> if (n = 1) then 1 else (n * (fac (n - 1)))
  let main: (int -> int) =fac 

  $ cat << EOF | ./REPL.exe  -stru -
  > fun f -> fun x -> f (f x)
  > EOF
  "fun f -> fun x -> f (f x)"
  Error: : end_of_input
  $ cat << EOF | ./REPL.exe  -stru -
  > fun x -> let v = x in v
  > EOF
  "fun x -> let v = x in v"
  Error: : end_of_input
  $ cat << EOF | ./REPL.exe  -stru -
  > let add = fun x -> fun  y -> x + y
  > let add1 = add 1
  > let main = add1 13
  > EOF
  "let add = fun x -> fun  y -> x + y\nlet add1 = add 1\nlet main = add1 13"
  Parsed.
  let add: (int -> (int -> int)) =fun x -> fun y -> (x + y)
  let add1: (int -> int) =(add 1)
  let main: int =(add1 13) 

  $ cat << EOF | ./REPL.exe  -stru -
  > let add = fun x -> x + x
  > let add1 = add 1
  > let main = add 1
  > EOF
  "let add = fun x -> x + x\nlet add1 = add 1\nlet main = add 1"
  Parsed.
  let add: (int -> int) =fun x -> (x + x)
  let add1: int =(add 1)
  let main: int =(add 1) 
# tuples 

  $ cat << EOF | ./REPL.exe  -stru -
  > let twice = fun x -> (x,x)
  > EOF
  "let twice = fun x -> (x,x)"
  Parsed.
  let twice: ('_1 -> ('_1, '_1)) =fun x -> (x, x) 

  $ cat << EOF | ./REPL.exe -stru -v -
  > let foo x =
  >   let y = fun z -> z in
  >   (y 1, y true)
  > EOF
