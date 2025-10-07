#
  $ cat << EOF | ./REPL.exe -core-type -
  > int
  "int"
  Parsed: int

  $ cat << EOF | ./REPL.exe -core-type -
  > 'abc
  "'abc"
  Parsed: 'abc

  $ cat << EOF | ./REPL.exe -core-type -
  > ('a * 'b)
  "('a * 'b)"
  Parsed: ('a * 'b)

  $ cat << EOF | ./REPL.exe -core-type -
  > (int * 'a) list
  "(int * 'a) list"
  Parsed: ((int * 'a)) list

  $ cat << EOF | ./REPL.exe -core-type -
  > ('t -> bool) option
  "('t -> bool) option"
  Parsed: (('t -> bool)) option

  $ cat << EOF | ./REPL.exe -core-type -
  > 'a -> 'b
  "'a -> 'b"
  Parsed: ('a -> 'b)

  $ cat << EOF | ./REPL.exe -core-type -
  > ('a -> 'b) * ('c -> 'd)
  "('a -> 'b) * ('c -> 'd)"
  Parsed: (('a -> 'b) * ('c -> 'd))

  $ cat << EOF | ./REPL.exe -core-type -
  > ('a * 'b) -> ('c * 'd)
  "('a * 'b) -> ('c * 'd)"
  Parsed: (('a * 'b) -> ('c * 'd))
#
