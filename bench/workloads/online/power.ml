(* Benchmark workload: function with runtime-constant parameters (Phase 5).

   [outer] passes `id 5` and `id 3` into [inner], which calls `pow x exp 1` on
   every iteration.  Because `id 5` and `id 3` are function calls rather than
   IR constants, static `specialize` cannot specialize [inner] or [pow].  The
   online JIT observes `exp = 5` and `x = 3` in the entry argument sampling
   window and generates guarded specialized versions (`inner$gspec`, `pow$gspec`).

   result: bench_iters * 500 * 364; exit value modulo 256. *)

let bench_iters = 400

let id v = v + 0

let rec pow x n acc =
  if n = 0 then acc else pow x (n - 1) (acc * x + 1)

let rec inner k exp x acc =
  if k = 0 then acc
  else inner (k - 1) exp x (acc + pow x exp 1)

let rec outer m acc =
  if m = 0 then acc
  else outer (m - 1) (inner 500 (id 5) (id 3) acc)

let main = outer bench_iters 0
