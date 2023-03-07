  $ cat << EOF | ./REPL.exe -stru -
  > let foo = fun x -> fun y -> if x then y else true
  > EOF
  "let foo = fun x -> fun y -> if x then y else true"
  Parsed.
  let foo: (bool -> (bool -> bool)) =fun x -> fun y -> if x then y else true 
