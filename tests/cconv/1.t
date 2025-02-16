Discovered by @ns-58
  $ cat << EOF | ./REPL.exe -
  > let main =
  >  let k1 x = x in
  >  let k2 x = x in
  >  let f z u = k1 (k2 (z+u)) in
  >  f 1 2
  > EOF
  Parsed: let main = let k1 x = x in let k2 x = x in let f z u = k1 (k2 (z + u)) 
                                                     in f 1 2
  After CCovv.
  let f k2 k1 z u = k1 (k2 (z + u))
  let k2 x = x
  let k1 x = x
  let main = f k2 k1 1 2
