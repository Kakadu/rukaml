let revapply a f = f a
let apply f a = f a

let main =
  let z1 = revapply 21 print in
  let z2 = apply print 22 in
  0
