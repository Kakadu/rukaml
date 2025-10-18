Discovered by @ns-58
  $ cat << EOF | ./run.exe -
  > let ret x k =  k x
  > let main =
  >   let z k = 0 in
  >   ret 1 (fun x -> z (fun x -> 0))
  > EOF
  Parsed: let ret x k = k x
          let main = let z k = 0 in ret 1 (fun x -> z (fun x -> 0))
  After CCovv.
  let ret x k = k x
  let fresh_2 x = 0
  let fresh_1 z x = z fresh_2
  let z k = 0
  let main = ret 1 (fresh_1 z)
