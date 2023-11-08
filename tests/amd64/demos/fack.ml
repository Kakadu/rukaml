let fresh n k =
  k n
let id x = x
let main =
  let u = fresh 1 id in
  let v = print u in
  0
