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
  $ cat << EOF | ./REPL.exe -prio -
  > 1+(2*3)
  > EOF
  "1+(2*3)"
  Parsed: (1 + (2 * 3))
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

# match
  $ cat << EOF | ./REPL.exe -e -
  > match (x, y) with
  > | (x, y) -> (x, y)
  > | _ -> (y, x)
  "match (x, y) with\n| (x, y) -> (x, y)\n| _ -> (y, x)"
  Parsed: match (x, y) with
            | (x, y) -> (x, y)
            | _ -> (y, x)
            

  $ cat << EOF | ./REPL.exe -e -
  > match e with
  > | (f, (f, (f, (f, s)))) -> 4
  > | (f, (f, (f, s))) -> 3
  > | (f, (f, s)) -> 2
  > | (f, s) -> 1
  > | s -> 0
  "match e with\n| (f, (f, (f, (f, s)))) -> 4\n| (f, (f, (f, s))) -> 3\n| (f, (f, s)) -> 2\n| (f, s) -> 1\n| s -> 0"
  Parsed: match e with
            | (f, (f, (f, (f, s)))) -> 4
            | (f, (f, (f, s))) -> 3
            | (f, (f, s)) -> 2
            | (f, s) -> 1
            | s -> 0
            
  $ cat << EOF | ./REPL.exe -e -
  > match x with
  > | One x -> 1
  > | Two (x, y) -> 2
  > | Three (x, y, z) -> 3
  "match x with\n| One x -> 1\n| Two (x, y) -> 2\n| Three (x, y, z) -> 3"
  Parsed: match x with
            | One (x) -> 1
            | Two ((x, y)) -> 2
            | Three ((x, y, z)) -> 3
            

  $ cat << EOF | ./REPL.exe -e -
  > match x with
  > | _ -> if x then y else z
  "match x with\n| _ -> if x then y else z"
  Parsed: match x with
            | _ -> if x then y else z
            

  $ cat << EOF | ./REPL.exe -e -
  > if x then 
  >   match y with
  >   | _ -> y
  > else 
  >   match z with
  >   | _ -> Z
  "if x then \n  match y with\n  | _ -> y\nelse \n  match z with\n  | _ -> Z"
  Parsed: (if x then match y with
                       | _ -> y
                        else match z with
                               | _ -> Z
                               )

  $ cat << EOF | ./REPL.exe -e -
  > match f x with
  > | Some x -> g x
  > | None -> a b c
  "match f x with\n| Some x -> g x\n| None -> a b c"
  Parsed: match f x with
            | Some (x) -> g x
            | None -> a b c
            
#
