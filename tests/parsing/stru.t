  $ cat << EOF | ./REPL.exe -stru -
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF
  "let rec zed f x = f (zed f) x\nlet fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))\nlet main = zed fac"
  Parsed: let rec zed f x = f (zed f) xlet fac self n = if n = 1 then 1 
                                                        else n * (self (- n 1))
          let main = zed fac

  $ cat << EOF | ./REPL.exe  -stru -
  > let id = fun x -> x
  > let idd = fun x -> x
  > let main = (id idd) (id 1)
  > EOF
  "let id = fun x -> x\nlet idd = fun x -> x\nlet main = (id idd) (id 1)"
  Parsed: let id x = xlet idd x = xlet main = id idd (id 1)
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
  Parsed: let rec fix f = f (fix f)let fac self n = if n = 1 then 1 else n * (
                                                                    self 
                                                                    (- n 1))
          let main = fix fac

  $ cat << EOF | ./REPL.exe  -stru -
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF
  "let rec zed f x = f (zed f) x\nlet fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))\nlet main = zed fac"
  Parsed: let rec zed f x = f (zed f) xlet fac self n = if n = 1 then 1 
                                                        else n * (self (- n 1))
          let main = zed fac

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
  Parsed: let rec fac n = if n = 1 then 1 else n * (fac (- n 1))let main = 
                                                                  fac

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
  Parsed: let add x y = x + ylet add1 = add 1let main = add1 13

  $ cat << EOF | ./REPL.exe  -stru -
  > let add = fun x -> x + x
  > let add1 = add 1
  > let main = add 1
  > EOF
  "let add = fun x -> x + x\nlet add1 = add 1\nlet main = add 1"
  Parsed: let add x = x + xlet add1 = add 1let main = add 1
