  $ run () { ../../driver/driver.exe $1 --target cconv -o a.ml && cat a.ml; }

  $ run << EOF
  > let rec s f g x = f x (g x)
  > EOF
  let rec s f g x = f x (g x)

  $ run << EOF
  > let main = fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v))
  > EOF
  let fresh_4 x v = x x v
  let fresh_3 f x = f (fresh_4 x)
  let fresh_2 x v = x x v
  let fresh_1 f x = f (fresh_2 x)
  let main f = fresh_3 f (fresh_1 f)

  $ run << EOF
  > let rec f = fun n -> f
  > EOF
  let rec f n = f

  $ run << EOF
  > let rec fac = fun n -> n*fac
  > EOF
  let rec fac n = n * fac

  $ run << EOF
  > let sum x = let (a, b) = x in a+b
  > EOF
  let sum x = let (a, b) = x in a + b

CPS Factorial
  $ run << EOF
  > let rec fack n k =
  >  if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > EOF
  let fresh_1 n k m = k (n * m)
  let rec fack n k = if n = 1 then k 1 else fack (n - 1) (fresh_1 n k)

CPS Fibonacci
  $ run << EOF
  > let rec fibk n k =
  >  if n<1 then k 1 else fibk (n-1) (fun p -> fibk (n-2) (fun q -> k (p + q)))
  > EOF
  let fresh_2 p k q = k (p + q)
  let fresh_1 n k fibk p = fibk (n - 2) (fresh_2 p k)
  let rec fibk n k = if n < 1 then k 1 else fibk (n - 1) (fresh_1 n k fibk)

Polyvariadic uncurrying
  $ run << EOF
  > let two f (a,b) = f a b
  > let succ prev f (a,rest) = prev (f a) rest
  > let three = succ two
  > let four = succ three
  > EOF
  let two f (a, b) = f a b
  let succ prev f (a, rest) = prev (f a) rest
  let three = succ two
  let four = succ three

Polyvariadic currying
  $ run << EOF
  > let two f a b = f (a, b)
  > let succ prev f arg = prev (fun rest -> f (arg,rest))
  > let three = succ two
  > let four = succ three
  > EOF
  let two f a b = f (a, b)
  let fresh_1 f arg rest = f (arg, rest)
  let succ prev f arg = prev (fresh_1 f arg)
  let three = succ two
  let four = succ three

Polyvariadic map
  $ run << EOF
  > let two f (a,b) = (f a, f b)
  > let succ prev f (a, rest) = (f a, prev f rest)
  > let three = succ two
  > let four = succ three
  > let temp = two (fun x -> x) (1,2)
  > EOF
  let two f (a, b) = (f a, f b)
  let succ prev f (a, rest) = (f a, prev f rest)
  let three = succ two
  let four = succ three
  let fresh_1 x = x
  let temp = two fresh_1 (1, 2)

TODO: Following output is a little bit shitty
  $ run << EOF #-vcc
  > let fresh_1 n =
  >   let temp = (n=1) in
  >   temp
  > EOF
  let fresh_1 n = let temp = n = 1 in temp
