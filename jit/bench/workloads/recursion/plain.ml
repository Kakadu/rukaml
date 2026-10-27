(* Benchmark workload: plain recursion, no closures.

   The hot recursion is the [inner] loop with a fixed depth (1000), so the
   stack usage stays constant while [bench_iters] changes the amount of work.
   That keeps T(N) linear and lets bench/harness.c subtract the fixed process
   startup by fitting T(N) = a + b*N.

   [inner] uses the only shape rukaml's LLVM backend supports for a top-level
   recursive function: two arguments, fully applied on the recursive call.
   The call is not tail-call-optimised (rukaml emits no `tail` markers), so
   [inner] is bounded by the stack; bench/harness.c raises RLIMIT_STACK.

   result: bench_iters * sum(1..1000) = bench_iters * 500500; the driver exits
   with that value taken modulo 256. *)

let bench_iters = 1000

let rec inner n acc =
  if n = 0 then acc else inner (n - 1) (acc + n)

let rec outer m acc =
  if m = 0 then acc else outer (m - 1) (inner 1000 acc)

let main = outer bench_iters 0
