let rec fack n k = if n = 1 then k 1 else fack (n - 1) (fun m -> k (n * m))
