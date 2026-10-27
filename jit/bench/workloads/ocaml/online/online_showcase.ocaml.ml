(* OCaml twin of online/online_showcase.ml, compiled by ocamlopt/ocamlc.
   Same computation and same checksum (passed to [exit]); OCaml turns the tail
   calls into loops, so no stack-limit trick is needed here. *)

let bench_iters = 1000

let id x = x + 0

let step_a acc n = acc + n * 3 + 1
let step_b acc n = acc + n * 5 - 2

let choose flag = if flag = 0 then step_a else step_b

let addk k x = k + x

let rec fold f mode n acc =
  if n = 0 then acc
  else
    let acc1 =
      if mode = 0 then f acc n
      else if mode = 1 then f (acc + 1) n
      else f (acc + 2) n
    in
    fold f mode (n - 1) acc1

let rec outer m f mode acc =
  if m = 0 then acc
  else
    let bump = addk mode in
    let depth = if m = bench_iters then 8192 else 8193 in
    outer (m - 1) f mode (fold f mode depth (bump acc))

let () = exit ((outer bench_iters (choose (id 0)) (id 1) 0) land 0xff)
