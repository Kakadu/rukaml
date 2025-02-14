let addk k = k 0
let foo f x = f x
let main =
  let tmp = foo print in
  addk tmp
