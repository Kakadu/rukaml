let bench_iters = 1000

let rec loop n acc =
  if n = 0 then acc
  else if acc = n then loop (n - 1) (acc + 2 * n)
  else loop (n - 1) (acc - n)

let rec outer m acc =
  if m = 0 then acc else outer (m - 1) (loop 1000 acc)

let () = exit ((outer bench_iters 0) land 0xff)
