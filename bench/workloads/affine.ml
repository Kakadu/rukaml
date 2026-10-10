(* Benchmark workload: affine transforms partially applied to two constants.

   Like pipeline.ml, but every transform is a binary [fma] partially applied to
   both of its constants inside the loop, so each iteration builds a longer
   `alloc_closure -> applyN -> applyN -> applyN` chain. `specialize` collapses
   each one into a single direct call with both constants folded in, a wider
   win per site than the unary case of pipeline.ml.

   result: acc grows by 2*3 + 1*4 + 5*1 = 15 per inner iteration, i.e.
   15000000 for 1000 * 1000 iterations; exit value modulo 256. *)

let fma k a x = k * a + x

let rec inner n acc =
  if n = 0 then acc
  else
    let g1 = fma 2 3 in
    let g2 = fma 1 4 in
    let g3 = fma 5 1 in
    inner (n - 1) (g3 (g2 (g1 acc)))

let rec outer m acc =
  if m = 0 then acc else outer (m - 1) (inner 1000 acc)

let main = outer 1000 0
