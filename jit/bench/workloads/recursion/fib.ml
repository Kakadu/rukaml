(* Benchmark workload: two-argument recursive Fibonacci.

   Neither recursive argument in `fib (d + 1) (n - 1)` is a constant, so static
   `specialize` does not fire; `devirt` and `online` eliminate the per-call
   closure allocations and varargs dispatches.

   result: bench_iters * F(22) = bench_iters * 17711; exit value modulo 256. *)

let bench_iters = 20

let rec fib d n =
  if n < 2 then n else fib (d + 1) (n - 1) + fib (d + 2) (n - 2)

let rec outer m acc =
  if m = 0 then acc else outer (m - 1) (acc + fib m 22)

let main = outer bench_iters 0
