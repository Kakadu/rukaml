  $ cat << EOF | ./REPL.exe -long   -
  > (f.[0]+1)
  > EOF
  "(f.[0]+1)"
  Parsed: (f.[0] + 1)
  $ cat << EOF | ./REPL.exe -long   -
  > ((f).[0]+1)
  > EOF
  "((f).[0]+1)"
  Parsed: (f.[0] + 1)
  $ cat << EOF | ./REPL.exe -prio   -
  > (f).[0]+1
  > EOF
  "(f).[0]+1"
  Parsed: (f.[0] + 1)
  $ cat << EOF | ./REPL.exe -vb -
  > let get arr = (arr).[0]+1
  > EOF
  "let get arr = (arr).[0]+1"
  Parsed: let get arr = arr.[0] + 1
  $ cat << EOF | ./REPL.exe -vb -
  > let f x = x.[0] := 5
  > EOF
  "let f x = x.[0] := 5"
  Error: : end_of_input
