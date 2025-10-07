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
