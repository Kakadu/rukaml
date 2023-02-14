
# value binding
  $ cat << EOF | ./REPL.exe -vb  -
  > let main = 9
  > EOF
  "let main = 9"
  Parsed: let main = 9

  $ cat << EOF | ./REPL.exe -vb -
  > let main=(fun x -> f x)
  > EOF
  "let main=(fun x -> f x)"
  Parsed: let main x = f x
  $ cat << EOF | ./REPL.exe -vb -
  > let main f x = f x
  > EOF
  "let main f x = f x"
  Parsed: let main f x = f x
  $ cat << EOF | ./REPL.exe -vb -
  > let rec fac f x = if x<1 then 1 else x * fac (x-1)
  > EOF
  "let rec fac f x = if x<1 then 1 else x * fac (x-1)"
  Parsed: let rec fac f x = if x < 1 then 1 else x * (fac (x - 1))
  $ cat << EOF | ./REPL.exe -
  > let main = fun x -> 1+1
  > EOF
  "let main = fun x -> 1+1"
  Parsed: let main x = 1 + 1

  $ cat << EOF | ./REPL.exe -
  > let main = fun x -> if x then 1 else x
  > EOF
  "let main = fun x -> if x then 1 else x"
  Parsed: let main x = if x then 1 else x
  $ cat << EOF | ./REPL.exe -
  > let main = fun f -> (fun x -> f (x x)) (fun x -> f (x x))
  > EOF
  "let main = fun f -> (fun x -> f (x x)) (fun x -> f (x x))"
  Parsed: let main f = (fun x -> f (x x)) (fun x -> f (x x))
  $ cat << EOF | ./REPL.exe -
  > let main = (fun fix -> fun f -> f (fix f))
  > EOF
  "let main = (fun fix -> fun f -> f (fix f))"
  Parsed: let main fix f = f (fix f)

  $ cat << EOF | ./REPL.exe -
  > let rec s f g x = f x (g x)
  > EOF
  "let rec s f g x = f x (g x)"
  Parsed: let rec s f g x = f x (g x)
  $ cat << EOF | ./REPL.exe -
  > let main = fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v))
  > EOF
  "let main = fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v))"
  Parsed: let main f = (fun x -> f (fun v -> x x v)) (fun x -> f (fun v ->
                                                                  x x v))
  $ cat << EOF | ./REPL.exe -
  > let rec f = fun n -> f
  > EOF
  "let rec f = fun n -> f"
  Parsed: let rec f n = f

  $ cat << EOF | ./REPL.exe -
  > let rec fac = fun n -> n*fac
  > EOF
  "let rec fac = fun n -> n*fac"
  Parsed: let rec fac n = n * fac
