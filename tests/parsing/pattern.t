  $ cat << EOF | ./run.exe -pat -
  > _
  Parsed: _

  $ cat << EOF | ./run.exe -pat -
  > a, b, c
  Parsed: (a, b, c)

  $ cat << EOF | ./run.exe -pat -
  > (a, b, c)
  Parsed: (a, b, c)

  $ cat << EOF | ./run.exe -pat -
  > Some x, Some y
  Parsed: (Some (x), Some (y))

  $ cat << EOF | ./run.exe -pat -
  > (Some x, Some y)
  Parsed: (Some (x), Some (y))

  $ cat << EOF | ./run.exe -pat -
  > Some (x, y, z)
  Parsed: Some ((x, y, z))

  $ cat << EOF | ./run.exe -pat -
  > Just (Some None)
  Parsed: Just (Some (None))

  $ cat << EOF | ./run.exe -e -
  > (1, (2, 3))
  Parsed: (1, (2, 3))

  $ cat << EOF | ./run.exe -pat -
  > Just a, Just (b, c), Just (d, e, f)
  Parsed: (Just (a), Just ((b, c)), Just ((d, e, f)))
