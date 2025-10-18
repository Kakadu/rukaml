(*
test
  (targets amd64)
  (flags (-cps))
  (run
    (stdout
      "rukaml_print_int 2"
      "Total closure allocations: 43"))
*)

let rec fack n =
  if n = 0
  then fun k -> k 1
  else if n = 1
  then fun k -> k 1
  else (
    let h = fack (n - 1) in
    fun k -> h (fun a -> a * n))


let main =
  let u = print (fack 10 (fun x -> x)) in
  let t = closure_count () in
  0
