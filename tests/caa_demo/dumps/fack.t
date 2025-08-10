  $ cat ../bench/fack5.ml | ../../../back_rv64/RV64_compiler.exe -cps -dcps -stop-after cps 
  After CPS optimisations.
  let main =
    let rec fack n k1 = 
      if n = 0 then k1 (fun k k2 -> k 1 k2) 
      else if n = 1 then k1 (fun k k3 -> k 1 k3) 
           else fack 
                  (n - 1) 
                  (fun t4 -> k1 (fun k k5 -> t4 (fun a k6 -> k6 (a * n)) k5))
            
      in 
    fack 
      5 
      (fun t7 -> t7 
                   (fun x k8 -> k8 x) 
                   (fun t9 -> let u = print t9 in let t = closure_count () in 
                                                  (fun x -> x) 0)
                )
    

  $ cat ../bench/fack5.ml | ../../../back_rv64/RV64_compiler.exe -cps -call_arity -dcps -stop-after cps
  After CPS optimisations.
  let main =
    let rec fack n e10 k1 = 
      if n = 0 then e10 1 k1 
      else if n = 1 then e10 1 k1 else fack (n - 1) (fun a k6 -> k6 (a * n)) k1 
      in 
    fack 
      5 
      (fun x k8 -> k8 x) 
      (fun t9 -> let u = print t9 in let t = closure_count () in (fun x -> x) 0)
    
