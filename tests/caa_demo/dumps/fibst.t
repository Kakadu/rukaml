  $ cat ../bench/fibst5.ml | ../../../back_rv64/RV64_compiler.exe -cps -dcps -stop-after cps 
  After CPS optimisations.
  let main =
    let rec aux n k1 = 
      if n = 0 then k1 (fun st k2 -> k2 ((), st + 1)) 
      else if n = 1 then k1 (fun st k3 -> k3 ((), st + 1)) 
           else aux 
                  (n - 2) 
                  (fun t4 -> aux 
                               (n - 1) 
                               (fun t5 -> k1 (fun st k6 -> 
                                               t5 
                                                 st 
                                                 (fun t7 -> let (a, st) = 
                                                            t7 in t4 st k6)
                                               ))
                            )
            
      in 
    aux 
      5 
      (fun t8 -> t8 
                   0 
                   (fun t9 -> let (un, st) = t9 in let u = print st in 
                                                   let t = closure_count () in 
                                                   (fun x -> x) 0)
                )
    

  $ cat ../bench/fibst5.ml | ../../../back_rv64/RV64_compiler.exe -cps -call_arity -dcps -stop-after cps
  After CPS optimisations.
  let main =
    let rec aux n e10 k1 = 
      if n = 0 then k1 ((), e10 + 1) 
      else if n = 1 then k1 ((), e10 + 1) 
           else aux 
                  (n - 1) 
                  e10 
                  (fun t7 -> let (a, st) = t7 in aux (n - 2) st k1)
            
      in 
    aux 
      5 
      0 
      (fun t9 -> let (un, st) = t9 in let u = print st in let t = closure_count 
                                                                  () in 
                                                          (fun x -> x) 0)
    
