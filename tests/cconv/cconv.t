  $ cat << EOF | ./REPL.exe -
  > let rec s f g x = f x (g x)
  > EOF
  Parsed: let rec s f g x = f x (g x)
  After CCovv.
  let rec s f g x = f x (g x)
  $ cat << EOF | ./REPL.exe -
  > let main = fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v))
  > EOF
  Parsed: let main f = (fun x -> f (fun v -> x x v)) (fun x -> f (fun v ->
                                                                  x x v))
  After CCovv.
  let fresh_4 x v = x x v
  let fresh_3 f x = f (fresh_4 x)
  let fresh_2 x v = x x v
  let fresh_1 f x = f (fresh_2 x)
  let main f = fresh_3 f (fresh_1 f)
  $ cat << EOF | ./REPL.exe -
  > let rec f = fun n -> f
  > EOF
  Parsed: let rec f n = f
  After CCovv.
  let rec f n = f

  $ cat << EOF | ./REPL.exe -
  > let rec fac = fun n -> n*fac
  > EOF
  Parsed: let rec fac n = n * fac
  After CCovv.
  let rec fac n = n * fac

  $ cat << EOF | ./REPL.exe -
  > let sum x = let (a, b) = x in a+b
  > EOF
  Parsed: let sum x = let (a, b) = x in a + b
  Fatal error: exception Failure("not implemented 2")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from CConv.conv.(fun) in file "compiler/CConv.ml", line 260, characters 23-71
  Called from Dune__exe__REPL.run_single.(fun) in file "tests/cconv/REPL.ml", line 18, characters 26-64
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Dune__exe__REPL.run_single in file "tests/cconv/REPL.ml", line 14, characters 6-443
  Called from Dune__exe__REPL in file "tests/cconv/REPL.ml" (inlined), line 56, characters 13-81
  Called from Dune__exe__REPL in file "tests/cconv/REPL.ml", line 55, characters 2-112
  [2]

# CPS Factorial
  $ cat << EOF | ./REPL.exe -
  > let rec fack n k =
  >  if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > EOF
  Parsed: let rec fack n k = if n = 1 then k 1 else fack (n - 1) (fun m ->
                                                                  k (n * m))
  After CCovv.
  let fresh_1 n k m = k (n * m)
  let rec fack n k = if n = 1 then k 1 else fack (n - 1) (fresh_1 n k)

# CPS Fibonacci
  $ cat << EOF | ./REPL.exe -
  > let rec fibk n k =
  >  if n<1 then k 1 else fibk (n-1) (fun p -> fibk (n-2) (fun q -> k (p + q)))
  > EOF
  Parsed: let rec fibk n k = if n < 1 then k 1 else fibk (n - 1) (fun p ->
                                                                  fibk 
                                                                  (n - 2) 
                                                                  (fun 
                                                                  q -> 
                                                                      k 
                                                                      (p + q)))
  After CCovv.
  let fresh_2 p k q = k (p + q)
  let fresh_1 n k fibk p = fibk (n - 2) (fresh_2 p k)
  let rec fibk n k = if n < 1 then k 1 else fibk (n - 1) (fresh_1 n k fibk)

# Polyvariadic uncurrying
  $ cat << EOF | ./REPL.exe -
  > let two f (a,b) = f a b
  > let succ prev f (a,rest) = prev (f a) rest
  > let three = succ two
  > let four = succ three
  > EOF
  Parsed: let two f (a, b) = f a b
          let succ prev f (a, rest) = prev (f a) rest
          let three = succ two
          let four = succ three
  After CCovv.
  let two f (a, b) = f a b
  let succ prev f (a, rest) = prev (f a) rest
  let three = succ two
  let four = succ three

# Polyvariadic currying
  $ cat << EOF | ./REPL.exe -
  > let two f a b = f (a, b)
  > let succ prev f arg = prev (fun rest -> f (arg,rest))
  > let three = succ two
  > let four = succ three
  > EOF
  Parsed: let two f a b = f (a, b)
          let succ prev f arg = prev (fun rest -> f (arg, rest))
          let three = succ two let four = succ three
  After CCovv.
  let two f a b = f (a, b)
  let fresh_1 f arg rest = f (arg, rest)
  let succ prev f arg = prev (fresh_1 f arg)
  let three = succ two
  let four = succ three

# Polyvariadic map
  $ cat << EOF | ./REPL.exe -
  > let two f (a,b) = (f a, f b)
  > let succ prev f (a, rest) = (f a, prev f rest)
  > let three = succ two
  > let four = succ three
  > EOF
  Parsed: let two f (a, b) = (f a, f b)
          let succ prev f (a, rest) = (f a, prev f rest)
          let three = succ two
          let four = succ three
  After CCovv.
  let two f (a, b) = (f a, f b)
  let succ prev f (a, rest) = (f a, prev f rest)
  let three = succ two
  let four = succ three
