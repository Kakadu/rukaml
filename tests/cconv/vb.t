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
  let fresh_2 f x = f (fun v -> x x v)
  let fresh_1 f x = f (fun v -> x x v) let main f = fresh_2 f (fresh_1 f)
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
# CPS tests
  $ cat << EOF | ./REPL.exe -
  > let rec fack n k =
  >  if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > EOF
  Parsed: let rec fack n k = if n = 1 then k 1 else fack (- n 1) (fun m ->
                                                                  k (n * m))
  After CCovv.
  let fresh_1 n k m = k (n * m)
  let rec fack n k = if n = 1 then k 1 else fack (- n 1) (fresh_1 n k)
#next one looks buggy
  $ cat << EOF | ./REPL.exe -
  > let rec fibk n k =
  >  if n<1 then k 1 else fibk (n-1) (fun p -> fibk (n-2) (fun q -> k (p + q)))
  > EOF
  Parsed: let rec fibk n k = if n < 1 then k 1 else fibk (- n 1) (fun p ->
                                                                  fibk 
                                                                  (- n 2) 
                                                                  (fun q ->
                                                                   k (p + q)))
  After CCovv.
  let fresh_2 k q = k (p + q)
  let fresh_1 n k p = fibk (- n 2) (fresh_2 k)
  let rec fibk n k = if n < 1 then k 1 else fibk (- n 1) (fresh_1 n k)
