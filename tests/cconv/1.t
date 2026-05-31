  $ run () { ../../driver/driver.exe $1 --target cconv -o a.ml && cat a.ml; }

Discovered by @ns-58
  $ run << EOF
  > let main =
  >  let k1 x = x in
  >  let k2 x = x in
  >  let f z u = k1 (k2 (z+u)) in
  >  f 1 2
  > EOF
  let f k2 k1 z u = k1 (k2 (z + u))
  let k2 x = x
  let k1 x = x
  let main = f k2 k1 1 2
