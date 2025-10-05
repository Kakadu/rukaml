  $ cat << EOF | ./REPL.exe -stru -
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF
  "let rec zed f x = f (zed f) x\nlet fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))\nlet main = zed fac"
  Parsed: let rec zed f x = f (zed f) x
  let fac self n = if n = 1 then 1 
                                                         else n * (self (n - 1))
  
  let main = zed fac
  

  $ cat << EOF | ./REPL.exe  -stru -
  > let id = fun x -> x
  > let idd = fun x -> x
  > let main = (id idd) (id 1)
  > EOF
  "let id = fun x -> x\nlet idd = fun x -> x\nlet main = (id idd) (id 1)"
  Parsed: let id x = x
  let idd x = x
  let main = id idd (id 1)
  
  $ cat << EOF | ./REPL.exe  -stru -
  > let rec fix f = f (fix f)
  > EOF
  "let rec fix f = f (fix f)"
  Parsed: let rec fix f = f (fix f)
  

  $ cat << EOF | ./REPL.exe -stru  -
  > let rec fix f = f (fix f)
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = fix fac
  > EOF
  "let rec fix f = f (fix f)\nlet fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))\nlet main = fix fac"
  Parsed: let rec fix f = f (fix f)
  let fac self n = if n = 1 then 1 else n * (
                                                                     self 
                                                                     (n - 1))
  
  let main = fix fac
  

  $ cat << EOF | ./REPL.exe  -stru -
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF
  "let rec zed f x = f (zed f) x\nlet fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))\nlet main = zed fac"
  Parsed: let rec zed f x = f (zed f) x
  let fac self n = if n = 1 then 1 
                                                         else n * (self (n - 1))
  
  let main = zed fac
  

  $ cat << EOF | ./REPL.exe  -stru -
  > (fun fix -> fun f -> f (fix f))
  > EOF
  "(fun fix -> fun f -> f (fix f))"
  Error: : string
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
  Parsed: let rec fac n = if n = 1 then 1 else n * (fac (n - 1))
  let main = 
                                                                   fac
  

  $ cat << EOF | ./REPL.exe  -stru -
  > fun f -> fun x -> f (f x)
  > EOF
  "fun f -> fun x -> f (f x)"
  Error: : string
  $ cat << EOF | ./REPL.exe  -stru -
  > fun x -> let v = x in v
  > EOF
  "fun x -> let v = x in v"
  Error: : string
  $ cat << EOF | ./REPL.exe  -stru -
  > let add = fun x -> fun  y -> x + y
  > let add1 = add 1
  > let main = add1 13
  > EOF
  "let add = fun x -> fun  y -> x + y\nlet add1 = add 1\nlet main = add1 13"
  Parsed: let add x y = x + y
  let add1 = add 1
  let main = add1 13
  

  $ cat << EOF | ./REPL.exe  -stru -
  > let add = fun x -> x + x
  > let add1 = add 1
  > let main = add 1
  > EOF
  "let add = fun x -> x + x\nlet add1 = add 1\nlet main = add 1"
  Parsed: let add x = x + x
  let add1 = add 1
  let main = add 1
  

  $ cat << EOF | ./REPL.exe  -stru -
  > let double = fun x -> (x, x)
  > EOF
  "let double = fun x -> (x, x)"
  Parsed: let double x = (x, x)
  
# patterns
  $ cat << EOF | ./REPL.exe -pat -
  > (x,y,z)
  > EOF
  "(x,y,z)"
  Parsed: (x, y, z)
  $ cat << EOF | ./REPL.exe -pat -
  > x
  > EOF
  "x"
  Parsed: x

  $ cat << EOF | ./REPL.exe -stru -
  > let fst (x,y) = x
  > let snd (x,y) = y
  > let swap (x,y) = (y,x)
  > EOF
  "let fst (x,y) = x\nlet snd (x,y) = y\nlet swap (x,y) = (y,x)"
  Parsed: let fst (x, y) = x
  let snd (x, y) = y
  let swap (x, y) = (y, x)
  
  $ cat << EOF | ./REPL.exe -prio -
  >   let (a,b) = swap p in
  >   a+b
  > EOF
  "  let (a,b) = swap p in\n  a+b"
  Parsed: let (a, b) = swap p in a + b

  $ cat << EOF | ./REPL.exe -stru -
  > let swap (a,b) = (b,a)
  > let resum p =
  >   let (a,b) = swap p in
  >   a+b
  > EOF
  "let swap (a,b) = (b,a)\nlet resum p =\n  let (a,b) = swap p in\n  a+b"
  Parsed: let swap (a, b) = (b, a)
  let resum p = let (a, b) = swap p in 
                                                 a + b
  


# CPS tests
  $ cat << EOF | ./REPL.exe -stru -
  > let rec fack n k =
  >  if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > EOF
  "let rec fack n k =\n if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))"
  Parsed: let rec fack n k = if n = 1 then k 1 else fack (n - 1) (fun m ->
                                                                  k (n * m))
  
  $ cat << EOF | ./REPL.exe -stru -
  > let rec fibk n k =
  >  if n<1 then k 1 else fibk (n-1) (fun p -> fibk (n-2) (fun q -> k (p + q)))
  > EOF
  "let rec fibk n k =\n if n<1 then k 1 else fibk (n-1) (fun p -> fibk (n-2) (fun q -> k (p + q)))"
  Parsed: let rec fibk n k = if n < 1 then k 1 else fibk (n - 1) (fun p ->
                                                                  fibk 
                                                                  (n - 2) 
                                                                  (fun 
                                                                  q -> 
                                                                      k 
                                                                      (p + q)))
  
