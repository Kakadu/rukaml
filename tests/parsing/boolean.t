  $ cat << EOF | ./REPL.exe -prio -
  > true
  > EOF
  "true"
  Parsed: true
  $ cat << EOF | ./REPL.exe -prio -
  > if true then 1 else 2
  > EOF
  "if true then 1 else 2"
  Parsed: (if true then 1 else 2)
  $ cat << EOF | ./REPL.exe -prio -
  > fun x -> fun y -> if x then y else true
  > EOF
  "fun x -> fun y -> if x then y else true"
  Parsed: (fun x -> (fun y -> if x then y else true))
  $ cat << EOF | ./REPL.exe -prio -
  > x && y ||
  > x && y
  > EOF
  "x && y ||\nx && y"
  Error: : end_of_input
  $ cat << EOF | ./REPL.exe -prio -
  > fun x -> if x then printint 52 else ()
  "fun x -> if x then printint 52 else ()"
  Parsed: (fun x -> if x then printint 52 else ())
