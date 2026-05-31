  $ cat << EOF | ./run.exe -pat -
  > _
  > EOF
  Parsed: _

  $ cat << EOF | ./run.exe -pat -
  > a, b, c
  > EOF
  Parsed: a, b, c

  $ cat << EOF | ./run.exe -pat -
  > (a, b, c)
  > EOF
  Parsed: a, b, c

  $ cat << EOF | ./run.exe -pat -
  > Some x, Some y
  > EOF
  Parsed: Some x, Some y

  $ cat << EOF | ./run.exe -pat -
  > (Some x, Some y)
  > EOF
  Parsed: Some x, Some y

  $ cat << EOF | ./run.exe -pat -
  > Some (x, y, z)
  > EOF
  Parsed: Some (x, y, z)

  $ cat << EOF | ./run.exe -pat -
  > Just (Some None)
  > EOF
  Parsed: Just (Some None)

  $ cat << EOF | ./run.exe -e -
  > (1, (2, 3))
  > EOF
  Parsed: 1, (2, 3)

  $ cat << EOF | ./run.exe -pat -
  > Just a, Just (b, c), Just (d, e, f)
  > EOF
  Parsed: Just a, Just (b, c), Just (d, e, f)
