let wrap f = if 1 = 1 then f else f

let test3 a b c =
  let a1 = print a in
  let b1 = print b in
  let c1 = print c in
  0
let test1 a =
  let a1 = print a in
  0

let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

let main =
  let temp2 = wrap test1 1 in
  0
