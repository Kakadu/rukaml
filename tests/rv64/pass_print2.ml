let addk x y k = k (x + y)

let foo print x = print x
let main =
  let tmp = foo print in
  addk 1 3 tmp
