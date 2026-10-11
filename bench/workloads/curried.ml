(* Benchmark workload: partial application in a hot loop.

   Each iteration partially applies [add] to 1 — a fresh closure — and then
   fully applies it to the accumulator. That is exactly the
   `alloc_closure -> applyN*` chain the `specialize` pass collapses into a
   direct call to a clone with the constant 1 baked in (`add$spec$0`), so
   this workload is where the optimization shows.

   result: acc = n, i.e. 1000000 for n = 1000000. *)

let add a b = a + b

let rec loop n acc =
  if n = 0 then acc else let f = add 1 in loop (n - 1) (f acc)

let main = loop 1000000 0
