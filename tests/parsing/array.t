# char array
  $ cat << EOF | ./run.exe -e -
  > ""
  > EOF
  Parsed: ""
  $ cat << EOF | ./run.exe -e -
  > "asdf"
  > EOF
  Parsed: "asdf"

# get sugar
  $ cat << EOF | ./run.exe -e -
  > r.(0)
  > EOF
  Parsed: (get r 0)

  $ cat << EOF | ./run.exe -e -
  > let r = "foobar" in
  > print (char_code r.(0))
  > EOF
  Parsed: let r = "foobar" in print (char_code (get r 0))

# set sugar
  $ cat << EOF | ./run.exe -e -
  > r.(123) <- 123
  > EOF
  Parsed: (set r 123 123)

  $ cat << EOF | ./run.exe -e -
  > let r = "Ocaml" in
  > r.(1) <- 'C'
  > EOF
  Parsed: let r = "Ocaml" in set r 1 'C'


