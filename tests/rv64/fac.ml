let fac n =
  let temp = (n=1) in 
  if temp
  then 1
  else (
    let p = n - 1 in
    let p2 = fac p in
    n * p2)

let main =
  let f = fac 4 in
  let g = print f in
  0