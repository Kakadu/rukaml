invalid input
  $ cat << EOF | ./REPL.exe -pat -
  > match
  "match"
  Error: : string

  $ cat << EOF | ./REPL.exe -pat -
  > with
  "with"
  Error: : string

  $ cat << EOF | ./REPL.exe -pat -
  > match x with
  "match x with"
  Error: : string

valid input
  $ cat << EOF | ./REPL.exe -long -
  > let (x, y) =
  >   match (x, y) with
  >   | (x, y) -> (x, y)
  > in
  > (x, y)
  "let (x, y) =\n  match (x, y) with\n  | (x, y) -> (x, y)\nin\n(x, y)"
  Parsed: let (x, y) = match (ETuple ((EVar "x"), (EVar "y"), [])) with
                         | (x, y) -> (ETuple ((EVar "x"), (EVar "y"), []))
                          in (x, y)

  $ cat << EOF | ./REPL.exe -long -
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
            
