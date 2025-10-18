(*
test
  (targets amd64)
  (flags (-cps))
  (run
    (stdout
      "rukaml_print_int 8"
      "Total closure allocations: 55"))
*)

let fibst m =
  let rec aux n =
    if n = 0
    then fun st -> ((), st + 1)
    else if n = 1
    then fun st -> ((), st + 1)
    else (
      let h = aux (n - 2) in
      let m = aux (n - 1) in
      fun st ->
        let (a, st) = m st in
        h st)
  in
  let (un, st) = aux m 0 in
  st

let main =
  let u = print (fibst 5) in
  let t = closure_count () in
  0
