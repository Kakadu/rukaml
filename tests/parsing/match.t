patterns
  $ cat << EOF | ./REPL.exe -pat -
  > _
  "_"
  Parsed: _

  $ cat << EOF | ./REPL.exe -pat -
  > (a, b, c)
  "(a, b, c)"
  Parsed: (a, b, c)

  $ cat << EOF | ./REPL.exe -pat -
  > Some x, Some y
  "Some x, Some y"
  Parsed: (Some (x), Some (y))

  $ cat << EOF | ./REPL.exe -pat -
  > (Some _, Some _)
  "(Some _, Some _)"
  Parsed: (Some (_), Some (_))

  $ cat << EOF | ./REPL.exe -pat -
  > Some (x, y, z)
  "Some (x, y, z)"
  Parsed: Some ((x, y, z))

  $ cat << EOF | ./REPL.exe -pat -
  > A (B (C, D(_)))
  "A (B (C, D(_)))"
  Parsed: A (B ((C, D (_))))

  $ cat << EOF | ./REPL.exe -pat -
  > Q(W(a, b), E(c, d, e))
  "Q(W(a, b), E(c, d, e))"
  Parsed: Q ((W ((a, b)), E ((c, d, e))))

  $ cat << EOF | ./REPL.exe -pat -
  > _, (_, _), (_, _, _)
  "_, (_, _), (_, _, _)"
  Parsed: (_, (_, _), (_, _, _))

  $ cat << EOF | ./REPL.exe -pat -
  > Just a, Just (b, c), Just (d, e, f)
  "Just a, Just (b, c), Just (d, e, f)"
  Parsed: (Just (a), Just ((b, c)), Just ((d, e, f)))

invalid input
  $ cat << EOF | ./REPL.exe -e -
  > match
  "match"
  Error: : can't parse many expressions

  $ cat << EOF | ./REPL.exe -e -
  > with
  "with"
  Error: : can't parse many expressions

  $ cat << EOF | ./REPL.exe -e -
  > match x with
  "match x with"
  Error: : can't parse many expressions

valid input
  $ cat << EOF | ./REPL.exe -e -
  > match (x, y) with
  > | (x, y) -> (x, y)
  "match (x, y) with\n| (x, y) -> (x, y)"
  Parsed: match (ETuple ((EVar "x"), (EVar "y"), [])) with
            | (x, y) -> (ETuple ((EVar "x"), (EVar "y"), []))
            

  $ cat << EOF | ./REPL.exe -e -
  > match e with
  > | (f, (f, (f, (f, s)))) -> 4
  > | (f, (f, (f, s))) -> 3
  > | (f, (f, s)) -> 2
  > | (f, s) -> 1
  > | s -> 0
  "match e with\n| (f, (f, (f, (f, s)))) -> 4\n| (f, (f, (f, s))) -> 3\n| (f, (f, s)) -> 2\n| (f, s) -> 1\n| s -> 0"
  Parsed: match (EVar "e") with
            | (f, (f, (f, (f, s)))) -> (EConst (PConst_int 4))
            | (f, (f, (f, s))) -> (EConst (PConst_int 3))
            | (f, (f, s)) -> (EConst (PConst_int 2))
            | (f, s) -> (EConst (PConst_int 1))
            | s -> (EConst (PConst_int 0))
            
  $ cat << EOF | ./REPL.exe -e -
  > match x with
  > | _ -> a
  > | x -> b
  "match x with\n| _ -> a\n| x -> b"
  Parsed: match (EVar "x") with
            | _ -> (EVar "a")
            | x -> (EVar "b")
            
  $ cat << EOF | ./REPL.exe -e -
  > match x with
  > | Some x -> Some x
  > | None -> None
  "match x with\n| Some x -> Some x\n| None -> None"
  Parsed: match (EVar "x") with
            | Some (x) -> (EApp ((EVar "Some"), (EVar "x")))
            | None -> (EVar "None")
            

  $ cat << EOF | ./REPL.exe -e -
  > match x with
  > | _ -> if x then y else z
  "match x with\n| _ -> if x then y else z"
  Parsed: match (EVar "x") with
            | _ -> (EIf ((EVar "x"), (EVar "y"), (EVar "z")))
            

  $ cat << EOF | ./REPL.exe -e -
  > if x then 
  >   match y with
  >   | _ -> y
  > else 
  >   match z with
  >   | _ -> Z
  "if x then \n  match y with\n  | _ -> y\nelse \n  match z with\n  | _ -> Z"
  Parsed: (if x then match (EVar "y") with
                       | _ -> (EVar "y")
                        else match (EVar "z") with
                               | _ -> (EVar "Z")
                               )
