let fib n =
  let temp = n < 2 in
  if temp
  then n
  else (
    let p1 = fib (n - 1) in
    let p2 = fib (n - 2) in
    p1 + p2)

let main =
  let f = fib 6 in
  let g = print f in
  0
