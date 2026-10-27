(* OCaml twin of plain.ml, compiled by ocamlopt/ocamlc from bench/harness.c.
   Same computation and same checksum (passed to [exit]); OCaml turns the tail
   call into a loop, so no stack-limit trick is needed here. *)

let bench_iters = 1000

let rec inner n acc = if n = 0 then acc else inner (n - 1) (acc + n)

let rec outer m acc = if m = 0 then acc else outer (m - 1) (inner 1000 acc)

let () = exit (outer bench_iters 0 land 255)
