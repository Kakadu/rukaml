(*
test
  (targets amd64)
  (flags (--cps))
  (run
    (stdout
      "rukaml_print_int 987"
      "Total closure allocations: 7895"))
*)

let rec fibk n =
  if n = 0
  then fun k -> k 1
  else if n = 1
  then fun k -> k 1
  else (
    let h = fibk (n - 1) in
    fun k -> h (fun l -> fibk (n - 2) (fun r -> k (l + r))))

let main =
  let u = print (fibk 15 (fun x -> x)) in
  let t = closure_count () in
  0
