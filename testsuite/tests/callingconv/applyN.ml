(*
test
  (targets rv64)
  (run (stdout 55 550 5500))
*)

(* We pass first 7 arguments via regs and others on heap *)
let apply10 f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
let func x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
let main =
  let test7_3 = apply10 func 1 2 3 4 5 6 in
  let() = printf "%d\n" (test7_3 7 8 9 10) in
  
  let test8_2 = apply10 func 10 20 30 40 50 60 70 80 in
  let() = printf "%d\n" (test8_2 90 100) in
  
  let test9_1 = apply10 func 100 200 300 400 500 600 700 800 900 in
  let() = printf "%d\n" (test9_1 1000) in
  0