  $ cat << EOF | ./run.exe -prio -
  > 1+2
  > EOF
  Parsed: 1 + 2

  $ cat << EOF | ./run.exe -prio -
  > (1+2)
  > EOF
  Parsed: 1 + 2

  $ cat << EOF | ./run.exe -long -
  > a 3
  > EOF
  Parsed: a 3

  $ cat << EOF | ./run.exe -prio -
  > 1+(2*3)
  > EOF
  Parsed: 1 + (2 * 3)

  $ cat << EOF | ./run.exe -prio -
  > (fun x -> x)
  > EOF
  Parsed: fun x -> x

  $ cat << EOF | ./run.exe -prio -
  > fun x -> x
  > EOF
  Parsed: fun x -> x

  $ cat << EOF | ./run.exe -long -
  > 1+(2*3)
  > EOF
  Error: : end_of_input

  $ cat << EOF | ./run.exe -long -
  > if 1 then 2 else 3
  > EOF
  Parsed: if 1 then 2 else 3

  $ cat << EOF | ./run.exe -long -
  > (fun f -> fun x -> f x)
  > EOF
  Parsed: fun f -> fun x -> f x

  $ cat << EOF | ./run.exe -long -
  > (fun x -> x)(fun x -> 2)(fun x -> 1)
  > EOF
  Parsed: (fun x -> x) (fun x -> 2) (fun x -> 1)

tuples
  $ cat << EOF | ./run.exe -prio -
  > (1,2,3) + (4,5)
  > EOF
  Parsed: (1, 2, 3) + (4, 5)

chars
  $ cat << EOF | ./run.exe -prio -
  > (fun x -> 'a') 'x'
  > EOF
  Parsed: (fun x -> 'a') 'x'

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
  Parsed: fac y

  $ cat << EOF | ./run.exe -e -
  > if 1 then 2 else x * fac (y-1)
  > EOF
  Parsed: if 1 then 2 else x * fac (y - 1)

constructors
  $ cat << EOF | ./run.exe -e -
  > Some (1, 2, 3, 4)
  > EOF
  Parsed: Some (1, 2, 3, 4)

  $ cat << EOF | ./run.exe -e -
  > Some (fun x -> x)
  > EOF
  Parsed: Some (fun x -> x)

  $ cat << EOF | ./run.exe -e -
  > Some (if true then 1 else 0)
  > EOF
  Parsed: Some (if true then 1 else 0)

  $ cat << EOF | ./run.exe -e -
  > Some (match true with _ -> true)
  > EOF
  Parsed: Some (match true with
                  | _ -> true)

  $ cat << EOF | ./run.exe -e -
  > Some [ 1; 2; 3 ]
  > EOF
  Parsed: Some [ 1; 2; 3 ]

  $ cat << EOF | ./run.exe -e -
  > Some (Some None)
  > EOF
  Parsed: Some (Some None)

  $ cat << EOF | ./run.exe -e -
  > f (Some x)
  > EOF
  Parsed: f (Some x)

  $ cat << EOF | ./run.exe -e -
  > f None x
  > EOF
  Parsed: f None x

match with
  $ cat << EOF | ./run.exe -e -
  > match (x, y) with
  > | (x, y) -> (x, y)
  > | _ -> (y, x)
  Parsed: match x, y with
            | x, y -> x, y
            | _ -> y, x

  $ cat << EOF | ./run.exe -e -
  > match e with
  > | (f, (f, (f, (f, s)))) -> 4
  > | (f, (f, (f, s))) -> 3
  > | (f, (f, s)) -> 2
  > | (f, s) -> 1
  > | s -> 0
  Parsed: match e with
            | f, (f, (f, (f, s))) -> 4
            | f, (f, (f, s)) -> 3
            | f, (f, s) -> 2
            | f, s -> 1
            | s -> 0
  $ cat << EOF | ./run.exe -e -
  > match x with
  > | One x -> 1
  > | Two (x, y) -> 2
  > | Three (x, y, z) -> 3
  Parsed: match x with
            | One x -> 1
            | Two (x, y) -> 2
            | Three (x, y, z) -> 3

  $ cat << EOF | ./run.exe -e -
  > match x with
  > | _ -> if x then y else z
  Parsed: match x with
            | _ -> if x then y else z

  $ cat << EOF | ./run.exe -e -
  > if x then 
  >   match y with
  >   | _ -> y
  > else 
  >   match z with
  >   | _ -> Z
  Parsed: if x then (match y with
                       | _ -> y) else match z with
                                        | _ -> Z

  $ cat << EOF | ./run.exe -e -
  > match f x with
  > | Some x -> g x
  > | None -> a b c
  Parsed: match f x with
            | Some x -> g x
            | None -> a b c

  $ cat << EOF | ./run.exe -e -
  > match x with
  > | A -> a
  > | B ->
  >   match y with
  >   | C -> c
  >   | D -> d
  Parsed: match x with
            | A -> a
            | B -> (match y with
                      | C -> c
                      | D -> d)

  $ cat << EOF | ./run.exe -e -
  > match x with
  > | A -> 
  >   (match y with
  >    | C -> c
  >    | D -> d)
  > | B -> b
  Parsed: match x with
            | A -> (match y with
                      | C -> c
                      | D -> d)
            | B -> b


  $ cat << EOF | ./run.exe -e -
  > match (match () with _ -> ()) with
  > | _ -> ()
  Parsed: match match () with
                  | _ -> () with
            | _ -> ()

  $ cat << EOF | ./run.exe -e -
  > match (if x then y else z) with
  > | _ -> ()
  Parsed: match (if x then y else z) with
            | _ -> ()

  $ cat << EOF | ./run.exe -e -
  > if (match x with _ -> ()) then 1 else 2
  Parsed: if (match x with
                | _ -> ()) then 1 else 2


# fixed issue: parsing tuples without parentheses
  $ cat << EOF | ./run.exe -e -
  > 1,2,3
  Parsed: 1, 2, 3

  $ cat << EOF | ./run.exe -e -
  > (1,2,3), (4, 5), 6
  Parsed: (1, 2, 3), (4, 5), 6

  $ cat << EOF | ./run.exe -e -
  > let x, y = 1, 2 in x + y
  Parsed: let (x, y) = 1, 2 in x + y

  $ cat << EOF | ./run.exe -e -
  > if true then 1, 2 else 2, 1
  Parsed: if true then 1, 2 else 2, 1

  $ cat << EOF | ./run.exe -e -
  > (if true then 1, 2 else 2), 1
  Parsed: (if true then 1, 2 else 2), 1

# edge cases

should pass
  $ cat << EOF | ./run.exe -e -
  > fun (x, y) -> x + y
  Parsed: fun (x, y) -> x + y

TODO
should fail
  $ cat << EOF | ./run.exe -e -
  > fun x, y -> x + y
  Parsed: fun (x, y) -> x + y

should pass
  $ cat << EOF | ./run.exe -e -
  > match 1, 2 with
  > | (x, y) -> x + y
  Parsed: match 1, 2 with
            | x, y -> x + y

should pass
  $ cat << EOF | ./run.exe -e -
  > match 1, 2 with
  > | x, y -> x + y
  Parsed: match 1, 2 with
            | x, y -> x + y

should pass
  $ cat << EOF | ./run.exe -e -
  > let (x, y) = 1, 2 in x, y
  Parsed: let (x, y) = 1, 2 in x, y

should pass
  $ cat << EOF | ./run.exe -e -
  > let x, y = 1, 2 in x, y
  Parsed: let (x, y) = 1, 2 in x, y

should pass
  $ cat << EOF | ./run.exe -e -
  > fun (Some x) -> x
  Parsed: fun (Some x) -> x

TODO
should fail
  $ cat << EOF | ./run.exe -e -
  > fun Some x -> x
  Parsed: fun (Some x) -> x

should pass
  $ cat << EOF | ./run.exe -e -
  > match Some 42 with
  > | (Some x) -> x
  Parsed: match Some 42 with
            | Some x -> x


should pass
  $ cat << EOF | ./run.exe -e -
  > match Some 42 with
  > | Some x -> x
  Parsed: match Some 42 with
            | Some x -> x

should pass
  $ cat << EOF | ./run.exe -e -
  > let Some x = Some 42 in x
  Parsed: let (Some x) = Some 42 in x

should pass
  $ cat << EOF | ./run.exe -e -
  > let (Some x) = Some 42 in x
  Parsed: let (Some x) = Some 42 in x
