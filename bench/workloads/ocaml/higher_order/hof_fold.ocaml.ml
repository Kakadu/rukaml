let bench_iters = 1000

let step acc x = acc + x * 3 + 1

let rec fold_range f n acc =
  if n = 0 then acc
  else fold_range f (n - 1) (f acc n)

let rec outer m f acc =
  if m = 0 then acc
  else outer (m - 1) f (fold_range f 1000 acc)

let () = exit ((outer bench_iters step 0) land 0xff)
