let f x =
  let z = print x in
  fun y -> y + 1


let main =
  if true
  then (
    let g = f 1 in
    let x = print 0 in
    g 2)
  else f 0 0

