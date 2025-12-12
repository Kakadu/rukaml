  $ cat << EOF | ./run.exe -vb  -
  > let main = 9
  > EOF
  Parsed: let main = 9

  $ cat << EOF | ./run.exe -vb -
  > let main=(fun x -> f x)
  > EOF
  Parsed: let main x = f x

  $ cat << EOF | ./run.exe -vb -
  > let main f x = f x
  > EOF
  Parsed: let main f x = f x

  $ cat << EOF | ./run.exe -vb -
  > let rec fac f x = if x<1 then 1 else x * fac (x-1)
  > EOF
  Parsed: let rec fac f x = if x < 1 then 1 else x * (fac (x - 1))

  $ cat << EOF | ./run.exe -
  > let main = fun x -> 1+1
  > EOF
  Parsed: let main x = 1 + 1

  $ cat << EOF | ./run.exe -
  > let main = fun x -> if x then 1 else x
  > EOF
  Parsed: let main x = if x then 1 else x

  $ cat << EOF | ./run.exe -
  > let main = fun f -> (fun x -> f (x x)) (fun x -> f (x x))
  > EOF
  Parsed: let main f = (fun x -> f (x x)) (fun x -> f (x x))

  $ cat << EOF | ./run.exe -
  > let main = (fun fix -> fun f -> f (fix f))
  > EOF
  Parsed: let main fix f = f (fix f)

  $ cat << EOF | ./run.exe -
  > let rec s f g x = f x (g x)
  > EOF
  Parsed: let rec s f g x = f x (g x)

  $ cat << EOF | ./run.exe -
  > let main = fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v))
  > EOF
  Parsed: let main f = (fun x -> f (fun v -> x x v)) (fun x -> f (fun v ->
                                                                  x x v))

  $ cat << EOF | ./run.exe -
  > let rec f = fun n -> f
  > EOF
  Parsed: let rec f n = f

  $ cat << EOF | ./run.exe -
  > let rec fac = fun n -> n*fac
  > EOF
  Parsed: let rec fac n = n * fac
