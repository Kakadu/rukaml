  $ cat << EOF | ./run.exe -prio -
  > 1+2
  > EOF
  Parsed: (1 + 2)

  $ cat << EOF | ./run.exe -prio -
  > (1+2)
  > EOF
  Parsed: (1 + 2)

  $ cat << EOF | ./run.exe -long -
  > a 3
  > EOF
  Parsed: (a 3)

  $ cat << EOF | ./run.exe -prio -
  > 1+(2*3)
  > EOF
  Parsed: (1 + (2 * 3))

  $ cat << EOF | ./run.exe -prio -
  > (fun x -> x)
  > EOF
  Parsed: (fun x -> x)

  $ cat << EOF | ./run.exe -prio -
  > fun x -> x
  > EOF
  Parsed: (fun x -> x)

  $ cat << EOF | ./run.exe -long -
  > 1+(2*3)
  > EOF
  Error: : end_of_input

  $ cat << EOF | ./run.exe -long -
  > if 1 then 2 else 3
  > EOF
  Parsed: (if 1 then 2 else 3)

  $ cat << EOF | ./run.exe -long -
  > (fun f -> fun x -> f x)
  > EOF
  Parsed: (fun f -> (fun x -> f x))

  $ cat << EOF | ./run.exe -long -
  > (fun x -> x)(fun x -> 2)(fun x -> 1)
  > EOF
  Parsed: ((fun x -> x) (fun x -> 2) (fun x -> 1))

tuples
  $ cat << EOF | ./run.exe -prio -
  > (1,2,3) + (4,5)
  > EOF
  Parsed: ((1, 2, 3) + (4, 5))

value binding
  $ cat << EOF | ./run.exe -vb  -
  > let main = 9
  > EOF
  Parsed: let main = 9

  $ cat << EOF | ./run.exe -vb -
  > let main=(fun x -> f x)
  > EOF
  Parsed: let main x = f x

  $ cat << EOF | ./run.exe -vb -
  > let main=(fun x -> let y = x in y)
  > EOF
  Parsed: let main x = let y = x in y

  $ cat << EOF | ./run.exe -prio -
  > y
  > EOF
  Parsed: y

  $ cat << EOF | ./run.exe -long -
  > fac (y)
  > EOF
  Parsed: (fac y)

  $ cat << EOF | ./run.exe -e -
  > if 1 then 2 else x * fac (y-1)
  > EOF
  Parsed: (if 1 then 2 else x * (fac (y - 1)))
