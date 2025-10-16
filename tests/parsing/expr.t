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
#

# match
  $ cat << EOF | ./run.exe -e -
  > match (x, y) with
  > | (x, y) -> (x, y)
  > | _ -> (y, x)
  "match (x, y) with\n| (x, y) -> (x, y)\n| _ -> (y, x)"
  Parsed: match (x, y) with
            | (x, y) -> (x, y)
            | _ -> (y, x)
            

  $ cat << EOF | ./run.exe -e -
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
            
  $ cat << EOF | ./run.exe -e -
  > match x with
  > | One x -> 1
  > | Two (x, y) -> 2
  > | Three (x, y, z) -> 3
  "match x with\n| One x -> 1\n| Two (x, y) -> 2\n| Three (x, y, z) -> 3"
  Parsed: match x with
            | One (x) -> 1
            | Two ((x, y)) -> 2
            | Three ((x, y, z)) -> 3
            

  $ cat << EOF | ./run.exe -e -
  > match x with
  > | _ -> if x then y else z
  "match x with\n| _ -> if x then y else z"
  Parsed: match x with
            | _ -> if x then y else z
            

  $ cat << EOF | ./run.exe -e -
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

  $ cat << EOF | ./run.exe -e -
  > match f x with
  > | Some x -> g x
  > | None -> a b c
  "match f x with\n| Some x -> g x\n| None -> a b c"
  Parsed: match f x with
            | Some (x) -> g x
            | None -> a b c
            
#
