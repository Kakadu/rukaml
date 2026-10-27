(* Benchmark workload: runtime-constant argument specialised by the online JIT,
   then collapsed by the optimizer.

   [step] receives `k = id 3` as an argument.  `id` is a function call, so
   static AOT and ocamlopt cannot turn `k` into a constant across the closure
   call.  The online JIT samples `k = 3`, emits `step.v1` with `k` folded in,
   and the update `acc + k * n` becomes a linear function that scalar
   evolution reduces to a closed form.  The whole benchmark then costs
   one multiplication per outer iteration.

   This is an algebraic collapse, not a steady-state loop speedup: most of the
   measured time is process startup and compilation.  bench/workloads/online/
   spec_nonlinear.ml is the variant without a closed form.

   Not part of `make bench`: the AOT row crashes at this size because `outer`
   is not tail-recursive (depth = bench_iters), so there is no valid AOT
   baseline to compare against.

   result: bench_iters * 3 * sum(1..1000) = bench_iters * 1501500; the driver
   exits with that value taken modulo 256. *)

let bench_iters = 200000

let id v = v + 0

let rec step k n acc =
  if n = 0 then acc else step k (n - 1) (acc + k * n)

let rec outer m acc =
  if m = 0 then acc else outer (m - 1) (step (id 3) 1000 acc)

let main = outer bench_iters 0
