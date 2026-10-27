let bench_iters = 20

let rec fib d n =
  if n < 2 then n else fib (d + 1) (n - 1) + fib (d + 2) (n - 2)

let rec outer m acc =
  if m = 0 then acc else outer (m - 1) (acc + fib m 22)

let () = exit ((outer bench_iters 0) land 0xff)
