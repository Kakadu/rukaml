(* OCaml twin of curried.ml. *)

let bench_iters = 1000

let add a b = a + b

let rec inner n acc =
  if n = 0 then acc else let f = add 1 in inner (n - 1) (f acc)

let rec outer m acc =
  if m = 0 then acc else outer (m - 1) (inner 1000 acc)

let () = exit (outer bench_iters 0 land 255)
