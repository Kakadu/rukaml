let id x = x

let k2 k1 t3 =
  let w = print 102 in
  k1 t3

let k3 fib n k t2 =
  let z = k2 k in
  let w = print 52 in
  fib (n - 2) z

let rec fib n k =
  if n < 2 then k n
  else
    let z = k3 fib n k in
    fib (n - 1) z

let main =
  let w = fib 2 id in
  let z = print w in
  0
