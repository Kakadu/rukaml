let rec fack n =
  if n = 0
  then fun k -> k 1
  else if n = 1
  then fun k -> k 1
  else (
    let h = fack (n - 1) in
    fun k -> h (fun a -> a * n))

let main =
  let u = print (fack 15 (fun x -> x)) in
  let t = closure_count () in
  0
