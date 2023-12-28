let fac n =
  if n = 1
  then 1
  else (
    let p = n - 1 in
    let p2 = fac p in
    n * p2)


let main =
  let n = fac 4 in
  0
