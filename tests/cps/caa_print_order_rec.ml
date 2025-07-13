let rec f x =
  if x < 2
  then fun y -> y + 1
  else (
    let h = f (x - 1) in
    let z = print x in
    h)


let main = f 4 1
