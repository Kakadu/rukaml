let id x = x
let rec fac_cps n k = if n=1 then k 1 else 
  let n1 = n-1 in 
  fac_cps n1 (fun p -> k (p*n))
  
let main =
  let f = fac_cps 4 id in
  let g = print f in
  0
