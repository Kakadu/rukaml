 let rec fac n u =
   if n=1 then 1 else fac (n-1) u *  n

let rec loop n j =
  let t = print n in
  if n=0
  then (let v = print 53 in 1)
  else ( loop (n+1) j)


let main =
  let t44 = loop 0 12 in
  let tmppppppp = print t44 in
  0
