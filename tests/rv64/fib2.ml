let return x = x
let main = 
  let rec fib n k1 = 
    if n < 2 then k1 n 
    else fib (n - 1) (fun t2 ->  fib (n - 2) (fun t3 -> k1 (t2 + t3))) 
  in 
  fib 6 (fun t4 -> let g = print t4 in return 0)
