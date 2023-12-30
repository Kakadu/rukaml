let fib a b n =
  let temp = n = 1 in
  if temp
  then b
  else (
    let sum = a + b in
    let n1 = n - 1 in
    fib b sum n1)

let main =
  let f = fib 1 1 6 in
  let g = print f in
  0
