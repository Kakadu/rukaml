(* Benchmark workload: runtime-constant specialisation without a closed form.

   Same shape as spec_linear.ml, but the update depends on the accumulator
   (`acc = n` selects the sign of the step), so the recursion is not linear
   and scalar evolution cannot collapse it.  The online JIT still samples
   `k = id 3` as a constant and specialises `step` for it; the gain has to come
   from the specialised body and the tail-call loop, not from algebra.

   result: the driver exits with the value taken modulo 256. *)

let bench_iters = 1000

let id v = v + 0

let rec step k n acc =
  if n = 0 then acc
  else if acc = n then step k (n - 1) (acc + k * n)
  else step k (n - 1) (acc - k * n)

let rec outer m acc =
  if m = 0 then acc else outer (m - 1) (step (id 3) 1000 acc)

let main = outer bench_iters 0
