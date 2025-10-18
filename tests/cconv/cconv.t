  $ cat << EOF | ./run.exe -
  > let rec s f g x = f x (g x)
  > EOF
  Parsed: let rec s f g x = f x (g x)
  After CCovv.
  let rec s f g x = f x (g x)
  $ cat << EOF | ./run.exe -
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
  $ cat << EOF | ./run.exe -
  > let rec f = fun n -> f
  > EOF
  Parsed: let rec f n = f
  After CCovv.
  let rec f n = f

  $ cat << EOF | ./run.exe -
  > let rec fac = fun n -> n*fac
  > EOF
  Parsed: let rec fac n = n * fac
  After CCovv.
  let rec fac n = n * fac

  $ cat << EOF | ./run.exe -
  > let sum x = let (a, b) = x in a+b
  > EOF
  Parsed: let sum x = let (a, b) = x in a + b
  After CCovv.
  let sum x = let (a, b) = x in a + b

CPS Factorial
  $ cat << EOF | ./run.exe -
  > let rec fack n k =
  >  if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > EOF
  Parsed: let rec fack n k = if n = 1 then k 1 else fack (n - 1) (fun m ->
                                                                  k (n * m))
  After CCovv.
  let fresh_1 n k m = k (n * m)
  let rec fack n k = if n = 1 then k 1 else fack (n - 1) (fresh_1 n k)

CPS Fibonacci
  $ cat << EOF | ./run.exe -
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

Polyvariadic uncurrying
  $ cat << EOF | ./run.exe -
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

Polyvariadic currying
  $ cat << EOF | ./run.exe -
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

Polyvariadic map
  $ cat << EOF | ./run.exe -
  > let two f (a,b) = (f a, f b)
  > let succ prev f (a, rest) = (f a, prev f rest)
  > let three = succ two
  > let four = succ three
  > let temp = two (fun x -> x) (1,2)
  > EOF
  Parsed: let two f (a, b) = (f a, f b)
          let succ prev f (a, rest) = (f a, prev f rest)
          let three = succ two
          let four = succ three
          let temp = two (fun x -> x) (1, 2)
  After CCovv.
  let two f (a, b) = (f a, f b)
  let succ prev f (a, rest) = (f a, prev f rest)
  let three = succ two
  let four = succ three
  let fresh_1 x = x
  let temp = two fresh_1 (1, 2)

TODO: Following output is a little bit shitty
  $ cat << EOF | ./run.exe - #-vcc
  > let fresh_1 n =
  >   let temp = (n=1) in
  >   temp
  > EOF
  Parsed: let fresh_1 n = let temp = n = 1 in temp
  After CCovv.
  let fresh_1 n = let temp = n = 1 in temp
