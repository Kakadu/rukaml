  $ cat << EOF | ./REPL.exe -prio -
  > 1+2
  > EOF
  "1+2"
  Parsed: (1 + 2)
  $ cat << EOF | ./REPL.exe -prio -
  > (1+2)
  > EOF
  "(1+2)"
  Parsed: (1 + 2)
  $ cat << EOF | ./REPL.exe -long -
  > a 3
  > EOF
  "a 3"
  Parsed: (a 3)
  $ cat << EOF | ./REPL.exe -prio -
  > 1+(2*3)
  > EOF
  "1+(2*3)"
  Parsed: (1 + (2 * 3))
  $ cat << EOF | ./REPL.exe -prio -
  > (fun x -> x)
  > EOF
  "(fun x -> x)"
  Parsed: (fun x -> x)
  $ cat << EOF | ./REPL.exe -prio -
  > fun x -> x
  > EOF
  "fun x -> x"
  Parsed: (fun x -> x)
  $ cat << EOF | ./REPL.exe -long -
  > 1+(2*3)
  > EOF
  "1+(2*3)"
  Error: : end_of_input
  $ cat << EOF | ./REPL.exe -long -
  > if 1 then 2 else 3
  > EOF
  "if 1 then 2 else 3"
  Parsed: (if 1 then 2 else 3)

  $ cat << EOF | ./REPL.exe -long -
  > (fun f -> fun x -> f x)
  > EOF
  "(fun f -> fun x -> f x)"
  Parsed: (fun f -> (fun x -> f x))
  $ cat << EOF | ./REPL.exe -long -
  > (fun x -> x)(fun x -> 2)(fun x -> 1)
  > EOF
  "(fun x -> x)(fun x -> 2)(fun x -> 1)"
  Parsed: ((fun x -> x) (fun x -> 2) (fun x -> 1))
# tuples
  $ cat << EOF | ./REPL.exe -prio -
  > (1,2,3) + (4,5)
  > EOF
  "(1,2,3) + (4,5)"
  Parsed: ((1, 2, 3) + (4, 5))

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
  > let main=(fun x -> let y = x in y)
  > EOF
  "let main=(fun x -> let y = x in y)"
  Parsed: let main x = let y = x in y
  $ cat << EOF | ./REPL.exe -prio -
  > y
  > EOF
  "y"
  Parsed: y
  $ cat << EOF | ./REPL.exe -long -
  > fac (y)
  > EOF
  "fac (y)"
  Parsed: (fac y)
  $ cat << EOF | ./REPL.exe -e -
  > if 1 then 2 else x * fac (y-1)
  > EOF
  "if 1 then 2 else x * fac (y-1)"
  Parsed: (if 1 then 2 else x * (fac (y - 1)))
#
