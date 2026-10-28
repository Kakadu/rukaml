(* Benchmark workload: the online-JIT showcase.

   This deliberately combines the three runtime facts that AOT cannot see:

   - [choose (id 0)] selects the closure target at run time;
   - [id 1] is a run-time-constant mode that guards three loop bodies;
   - [addk mode] is a fresh partial application in every outer iteration.

   AOT LLVM compiles [fold] once with [f] and [mode] as opaque parameters and
   keeps the generic `rukaml_applyN` path.  Online tier-up profiles the closure
   site (inline cache), the constant mode (guarded specialization) and the
   [addk] chain (devirtualization), then runs O2 on the resulting delta.

   [depth] is 8192 on the first outer iteration and 8193 on the rest, so the
   online JIT cannot fold the whole inner loop into a constant; the two values
   stay close enough to keep T(N) linear.  bench/harness.c substitutes
   [bench_iters] everywhere, so the comparison inside [outer] works after
   source generation.

   The harness checks the exit value modulo 256 against the AOT rows; the
   exact value is not needed to read the benchmark. *)

let bench_iters = 1000

let id x = x + 0

let step_a acc n = acc + n * 3 + 1
let step_b acc n = acc + n * 5 - 2

let choose flag =
  if flag = 0 then step_a else step_b

let addk k x = k + x

let rec fold f mode n acc =
  if n = 0 then acc
  else
    let acc1 =
      if mode = 0 then f acc n
      else if mode = 1 then f (acc + 1) n
      else f (acc + 2) n
    in
    fold f mode (n - 1) acc1

let rec outer m f mode acc =
  if m = 0 then acc
  else
    let bump = addk mode in
    let depth = if m = bench_iters then 8192 else 8193 in
    outer (m - 1) f mode (fold f mode depth (bump acc))

let main = outer bench_iters (choose (id 0)) (id 1) 0
