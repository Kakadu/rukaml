(* Benchmark workload: partial application in a hot loop.

   Each iteration partially applies [add] to 1 — a fresh closure — and then
   fully applies it to the accumulator. That is exactly the
   `alloc_closure -> applyN*` chain the `specialize` pass collapses into a
   direct call to a clone with the constant 1 baked in (`add$spec$0`), so
   this workload is where the optimization shows.

   The inner loop has a fixed depth so the stack stays constant while
   [bench_iters] scales the work; see ../recursion/plain.ml.

   result: acc = bench_iters * 1000. *)

let bench_iters = 1000

let add a b = a + b

let rec inner n acc =
  if n = 0 then acc else let f = add 1 in inner (n - 1) (f acc)

let rec outer m acc =
  if m = 0 then acc else outer (m - 1) (inner 1000 acc)

let main = outer bench_iters 0
