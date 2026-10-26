(* OCaml twin of affine.ml, compiled by ocamlopt/ocamlc from bench/harness.c.
   Same computation and same checksum (passed to [exit]). *)

let bench_iters = 1000

let rec inner n acc = if n = 0 then acc else inner (n - 1) (acc + 15)

let rec outer m acc = if m = 0 then acc else outer (m - 1) (inner 1000 acc)

let () = exit (outer bench_iters 0 land 255)
