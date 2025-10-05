  $ cat > input.ml <<- EOF
  > let rec fib n k =
  >   if n=0 then k 0 else if n=1 then k 1 else fib (n-2) (fun a -> fib (n-1) (fun b -> k(a+b)))
  > let main = let u = print_int (fib 8 (fun w -> w)) in 0
  > EOF
  $ cat input.ml | ./REPL.exe -stru -
  "let rec fib n k =\n  if n=0 then k 0 else if n=1 then k 1 else fib (n-2) (fun a -> fib (n-1) (fun b -> k(a+b)))\nlet main = let u = print_int (fib 8 (fun w -> w)) in 0"
  Parsed: let rec fib n k = if n = 0 then k 0 else if n = 1 then k 1 else 
                                                                     fib 
                                                                     (n - 2) 
                                                                     (fun 
                                                                     a ->
                                                                      fib 
                                                                      (n - 1) 
                                                                      (fun 
                                                                      b ->
                                                                       
                                                                      k 
                                                                      (a + b)))
  
                            let main = let u = print_int (fib 8 (fun w -> w)) 
                                                 in 0
  
