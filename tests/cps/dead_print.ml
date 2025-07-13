let f x =
  let y = print x in
  fun x -> x + 0


let main =
  let x = f 1 in
  let y = f 1 in
  0

