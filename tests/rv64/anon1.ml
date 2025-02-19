let main =
  let ret x k =  k x in
  let z k = 0 in
  ret 1 (fun x -> z (fun x -> 0))
