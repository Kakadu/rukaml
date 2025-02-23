let rec fac acc n = if n<2 then acc else
  let n1 = n-1 in
  let p1 = acc*n in
  fac p1 n1
let main =
  let f = fac 1 4 in
  let g = print f in
  0
