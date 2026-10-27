(* Benchmark workload: a self-recursive accumulator that only tail-call
   elimination can turn into a loop.

   [loop] is recursive through a closure-shaped call site: rukaml lowers the
   recursive call to `rukaml_alloc_closure(@loop, 2)` followed by
   `rukaml_applyN`.  Once `devirt` sees the known target it rewrites the chain
   into a direct `call @loop`, which makes the call tail-position self-recursion
   that `O2` (tailcallelim) can rewrite into a loop.

   The branch depends on the accumulator itself (`acc = n`), so
   scalar evolution cannot replace the recursion by a closed form; this keeps
   tail-call elimination as the transformation that does the work.

   [outer] runs [loop] many times, so the online JIT tiers up the hot function.

   result: the driver exits with the value taken modulo 256. *)

let bench_iters = 1000

let rec loop n acc =
  if n = 0 then acc
  else if acc = n then loop (n - 1) (acc + 2 * n)
  else loop (n - 1) (acc - n)

let rec outer m acc =
  if m = 0 then acc else outer (m - 1) (loop 1000 acc)

let main = outer bench_iters 0
