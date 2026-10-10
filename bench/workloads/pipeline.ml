(* Benchmark workload: a pipeline of constant partial applications in a hot
   loop.

   [inner] threads the accumulator through four unary transforms; every
   iteration builds four fresh `alloc_closure -> applyN*` chains by partially
   applying [inc]/[dec] to a constant and then applying the result. That is
   exactly the shape the JIT's `specialize` pass collapses into direct calls to
   clones with the constants baked in (`inc$spec$0`, `dec$spec$0`, ...): the
   optimized JIT pays four calls per iteration where the plain JIT pays four
   closure allocations and eight varargs dispatches. An AOT compiler cannot do
   the same devirtualization — it sees the closure as an opaque function
   pointer.

   The [outer] loop keeps the recursion shallow (about 2000 frames rather than
   a million) so the [inner] loop's hot code stays cache-resident and the
   per-iteration transform cost, not stack traffic, dominates.

   result: acc grows by 1 + 2 - 1 + 3 = 5 per inner iteration, i.e. 5000000 for
   1000 * 1000 iterations; the driver exits with that value modulo 256. *)

let inc a x = x + a
let dec a x = x - a

let rec inner n acc =
  if n = 0 then acc
  else
    let f1 = inc 1 in
    let f2 = inc 2 in
    let f3 = dec 1 in
    let f4 = inc 3 in
    inner (n - 1) (f4 (f3 (f2 (f1 acc))))

let rec outer m acc =
  if m = 0 then acc else outer (m - 1) (inner 1000 acc)

let main = outer 1000 0
