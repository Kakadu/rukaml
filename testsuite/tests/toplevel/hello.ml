(*
   test
  (targets amd64)
  (run (stdout 
          "hello from global constant"
          "hello from global evaluation"
          "hello from global matching"
          "hello from global function"
          "hello from main"))
*)

let printfn s = printf "%s\n" s
let c = printfn "hello from global constant"
let _ = printfn "hello from global evaluation"
let 4, () = 4, printfn "hello from global matching"
let f () = printfn "hello from global function"
let () = f ()

let main =
  let () = printfn "hello from main" in
  0
;;
