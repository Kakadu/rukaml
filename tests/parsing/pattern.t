  $ cat << EOF | ./REPL.exe -pat -
  > _
  "_"
  Parsed: _

  $ cat << EOF | ./REPL.exe -pat -
  > a, b, c
  "a, b, c"
  Parsed: (a, b, c)

  $ cat << EOF | ./REPL.exe -pat -
  > (a, b, c)
  "(a, b, c)"
  Parsed: (a, b, c)

  $ cat << EOF | ./REPL.exe -pat -
  > Some x, Some y
  "Some x, Some y"
  Parsed: (Some (x), Some (y))

  $ cat << EOF | ./REPL.exe -pat -
  > (Some x, Some y)
  "(Some x, Some y)"
  Parsed: (Some (x), Some (y))

  $ cat << EOF | ./REPL.exe -pat -
  > Some (x, y, z)
  "Some (x, y, z)"
  Parsed: Some ((x, y, z))

  $ cat << EOF | ./REPL.exe -pat -
  > Just (Some None)
  "Just (Some None)"
  Parsed: Just (Some (None))

  $ cat << EOF | ./REPL.exe -e -
  > (1, (2, 3))
  "(1, (2, 3))"
  Parsed: (1, (2, 3))

  $ cat << EOF | ./REPL.exe -pat -
  > Just a, Just (b, c), Just (d, e, f)
  "Just a, Just (b, c), Just (d, e, f)"
  Parsed: (Just (a), Just ((b, c)), Just ((d, e, f)))
