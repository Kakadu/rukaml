 let rec fibk n k =
  if n = 0 then  k 1
  else if n = 1 then k 1
  else (fibk (n-1) (fun l -> fibk (n-2) (fun r -> k (l + r)) ))

  let main =
  let u = print (fibk 5 (fun x -> x)) in
  let t = closure_count () in
  0

