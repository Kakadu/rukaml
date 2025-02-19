let revapply x k = k x
let const0 x = 0
let fresh1 z x = z const0
let z k = 0
let main =
  let u = fresh1 z in
  let w = print 255 in
  u 1