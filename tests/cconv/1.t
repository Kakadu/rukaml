  $ run () { ../../driver/driver.exe $1 --target cconv -o a.ml && cat a.ml; }

Discovered by @ns-58
  $ run << EOF
  > let main =
  >  let k1 x = x in
  >  let k2 x = x in
  >  let f z u = k1 (k2 (z+u)) in
  >  f 1 2
  > EOF
  let __lifted_let_1_f k2 k1 z u = k1 (k2 (z + u))
  let __lifted_let_2_k2 x = x
  let __lifted_let_3_k1 x = x
  let main = __lifted_let_1_f __lifted_let_2_k2 __lifted_let_3_k1 1 2
