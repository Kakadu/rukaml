let revapply x k = k x
let const0 x = 0
let fresh1 z x = z const0
let z k = 0
let main =
  let w = print 127 in
  let u = fresh1 z in
  let w = print 255 in
  revapply 1 u