let rec fac n = if n = 1 then 1 else n * fac (n - 1)
let main = print (fac 6)
