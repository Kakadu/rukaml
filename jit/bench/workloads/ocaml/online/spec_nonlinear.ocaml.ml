let bench_iters = 1000

let id v = v + 0

let rec step k n acc =
  if n = 0 then acc
  else if acc = n then step k (n - 1) (acc + k * n)
  else step k (n - 1) (acc - k * n)

let rec outer m acc =
  if m = 0 then acc else outer (m - 1) (step (id 3) 1000 acc)

let () = exit ((outer bench_iters 0) land 0xff)
