let bench_iters = 400

let id v = v + 0

let rec pow x n acc =
  if n = 0 then acc else pow x (n - 1) (acc * x + 1)

let rec inner k exp x acc =
  if k = 0 then acc
  else inner (k - 1) exp x (acc + pow x exp 1)

let rec outer m acc =
  if m = 0 then acc
  else outer (m - 1) (inner 500 (id 5) (id 3) acc)

let () = exit ((outer bench_iters 0) land 0xff)
