let fresh n k =
  let m =n-1 in 
  let mn = m*n in
  k mn
let id x = x
let main =
  let u = fresh 4 id in
  let v = print u in
  0
