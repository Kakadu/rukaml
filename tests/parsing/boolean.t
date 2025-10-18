  $ cat << EOF | ./run.exe -prio -
  > true
  > EOF
  Parsed: true

  $ cat << EOF | ./run.exe -prio -
  > if true then 1 else 2
  > EOF
  Parsed: (if true then 1 else 2)

  $ cat << EOF | ./run.exe -prio -
  > fun x -> fun y -> if x then y else true
  > EOF
  Parsed: (fun x -> (fun y -> if x then y else true))

  $ cat << EOF | ./run.exe -prio -
  > x && y ||
  > x && y
  > EOF
  Error: : end_of_input

  $ cat << EOF | ./run.exe -prio -
  > fun x -> if x then printint 52 else ()
  > EOF
  Parsed: (fun x -> if x then printint 52 else ())
