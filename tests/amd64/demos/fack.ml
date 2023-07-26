let rec fac n k = if n = 1 then k 1 else fac (n - 1) (fun u -> k (u * n))
let id u = u
let main = print (fac 4 id)
