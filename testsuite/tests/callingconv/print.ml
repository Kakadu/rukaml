(*
test
  (targets rv64)
  (run (stdout "1 2 3 4 5 | 6 7 8 9 10" 
               "1 2 3 4 5 | 6 7 8 9 10"
               "1 2 3 4 5 | 6 7 8 9 10 | 11 12 13 14 15 | 16 17 18 19 20"
               "1 2 3 4 5 | 6 7 8 9 10 | 11 12 13 14 15 | 16 17 18 19 20"
  ))
*)

(* We pass first 7 arguments via regs and others on heap *)
let main =
  let clos_printf_10 = printf "%d %d %d %d %d | %d %d %d %d %d\n" 1 2 3 4 5 6 7 in
  let u = clos_printf_10 8 9 10 in
  let clos_sprintf_10 = sprintf "%d %d %d %d %d | %d %d %d %d %d\n" 1 2 3 4 5 6 in
  let msg_sprintf_10 = clos_sprintf_10 7 8 9 10 in
  let k = printf "%s" msg_sprintf_10 in

  let clos_printf_20 = printf "%d %d %d %d %d | %d %d %d %d %d | %d %d %d %d %d | %d %d %d %d %d\n" 1 2 3 4 5 6 7 8 9 10 11 12 in
  let e = clos_printf_20 13 14 15 16 17 18 19 20 in

  let clos_sprintf_20 = sprintf "%d %d %d %d %d | %d %d %d %d %d | %d %d %d %d %d | %d %d %d %d %d\n" 1 2 3 4 5 6 7 8 9 10 11 in
  let msg_sprintf_20 = clos_sprintf_20 12 13 14 15 16 17 18 19 20 in
  let v = printf "%s" msg_sprintf_20 in
  0