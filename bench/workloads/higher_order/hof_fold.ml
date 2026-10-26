(* Benchmark workload: higher-order fold with a closure parameter (Phase 6).

   [fold_range] receives the binary function [f] as a parameter and invokes
   `f acc n` on every iteration.  Static `devirt` and `specialize` cannot
   resolve a parameter closure, whereas the online JIT profiles `f->code` at
   the `rukaml_applyN` call site and emits an inline guard calling `step`
   directly.

   result: bench_iters * sum_{n=1..1000} (3*n + 1) = bench_iters * 1502500;
   exit value modulo 256. *)

let bench_iters = 1000

let step acc x = acc + x * 3 + 1

let rec fold_range f n acc =
  if n = 0 then acc
  else fold_range f (n - 1) (f acc n)

let rec outer m f acc =
  if m = 0 then acc
  else outer (m - 1) f (fold_range f 1000 acc)

let main = outer bench_iters step 0
