let wrap f = if 1 = 1 then f else f

let test3 a b c =
  let t0 = print a in
  let t1 = print b in
  let t2 = print c in
  0

let test7 a b c d e f g = a + b + c + d + e + f + g
let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

let main =
  let temp1 =
    print
      (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
         1000000000)
  in
  let temp2 = wrap test3 1 10 10 in
  0
