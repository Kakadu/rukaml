(* Benchmark workload: plain recursion, no closures.

   [loop] uses the only shape rukaml's LLVM backend supports for a top-level
   recursive function: two arguments, fully applied on the recursive call.
   The call is not tail-call-optimised (rukaml emits no `tail` markers), so
   [n] is bounded by the stack; bench/harness.c raises RLIMIT_STACK.

   result: sum(1..n) = n*(n+1)/2, i.e. 500000500000 for n = 1000000; the
   driver exits with that value taken modulo 256. *)

let rec loop n acc =
  if n = 0 then acc else loop (n - 1) (acc + n)

let main = loop 1000000 0
