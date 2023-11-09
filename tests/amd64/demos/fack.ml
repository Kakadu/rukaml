let fack n k =
  let n1 = n-1 in 
  if n1=0 then k 1 else fack n1 (fun p -> k (p*n))
let id x = x
let main =
  let u = fack 4 id in
  let v = print u in
  0
