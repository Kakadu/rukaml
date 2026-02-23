  $ parse_expr () { ./run_ocaml.exe -parse -expr -o a.ml && cat a.ml; }

# ----- parser -----

  $ parse_expr << EOF
  > ;garbage
  > EOF
  parsing error: choice

  $ parse_expr << EOF
  > 1 + 2 * 3 - 4 < 5 || 6 / 7 >= 8
  parsed: (((1 + (2 * 3)) - 4) < 5) || ((6 / 7) >= 8)

  $ parse_expr << EOF
  > f x + g y < h z
  parsed: (f x + g y) < h z

  $ parse_expr << EOF
  > 1, true, ()
  > EOF
  parsed: 1, true, ()

  $ parse_expr << EOF
  > (1, 2, 3), (4, 5), 6
  > EOF
  parsed: (1, 2, 3), (4, 5), 6

  $ parse_expr << EOF
  > (fun x -> x) (fun y -> y)
  > EOF
  parsed: (fun x -> x) (fun y -> y)

  $ parse_expr << EOF
  > (fun x -> x), (fun x y -> x, y)
  > EOF
  parsed: (fun x -> x), (fun x -> fun y -> x, y)

  $ parse_expr << EOF
  > fun _ -> 1, 2
  > EOF
  parsed: fun _ -> 1, 2

  $ parse_expr << EOF
  > (fun _ -> 1), 2
  > EOF
  parsed: (fun _ -> 1), 2

  $ parse_expr << EOF
  > let a = b in 1, 2
  > EOF
  parsed: let a = b in 1, 2

  $ parse_expr << EOF
  > (let a = b in 1), 2
  > EOF
  parsed: (let a = b in 1), 2

  $ parse_expr << EOF
  > if true then 0 else 1
  > EOF
  parsed: if true then 0 else 1

  $ parse_expr << EOF
  > if true then (fun x -> x) else (fun y -> y)
  > EOF
  parsed: if true then fun x -> x else fun y -> y

  $ parse_expr << EOF
  > if true then fun x -> x else fun y -> y
  > EOF
  parsed: if true then fun x -> x else fun y -> y

  $ parse_expr << EOF
  > fun x -> if x then 0 else 1
  > EOF
  parsed: fun x -> if x then 0 else 1

  $ parse_expr << EOF
  > fun (x, y) -> if true then x else y
  > EOF
  parsed: fun (x, y) -> if true then x else y

  $ parse_expr << EOF
  > fun x -> if x then (fun y -> if y then 0 else 1) else (fun z -> if z then 1 else 0)
  > EOF
  parsed: fun x -> if x then fun y -> if y then 0 else 1 else fun z -> if z then 1 else 0

  $ parse_expr << EOF
  > let _ = () in ()
  > EOF
  parsed: let _ = () in ()

  $ parse_expr << EOF
  > let () = () in ()
  > EOF
  parsed: let () = () in ()

  $ parse_expr << EOF
  > let (1, 2) = () in ()
  > EOF
  parsed: let 1, 2 = () in ()

  $ parse_expr << EOF
  > let (1, 2, 3), (4, 5), 6 = () in ()
  > EOF
  parsed: let (1, 2, 3), (4, 5), 6 = () in ()

  $ parse_expr << EOF
  > let () = (1, 2, 3), (4, 5), 6 in ()
  > EOF
  parsed: let () = (1, 2, 3), (4, 5), 6 in ()

  $ parse_expr << EOF
  > let () = (fun x -> x) in ()
  > EOF
  parsed: let () = fun x -> x in ()

  $ parse_expr << EOF
  > let x = 1 in
  >   let y = 2 in
  >     x + y
  > EOF
  parsed: let x = 1 in let y = 2 in x + y

  $ parse_expr << EOF
  > let rec fact n =
  >   if n < 2 then 1 else n * fact (n - 1)
  > in
  > fact 5
  > EOF
  parsed: let rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1) in fact 5

  $ parse_expr << EOF
  > let rec fib n =
  >   if n < 2 then n else fib (n - 1) + fib (n - 2)
  > in
  > fib 5
  > EOF
  parsed: let rec fib = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 5
