  $ cat ../bench/fibk5.ml | ../../../back_rv64/RV64_compiler.exe -cps -dcps -stop-after cps 
  After CPS optimisations.
  let main =
    let rec fibk n k1 = 
      if n = 0 then k1 (fun k k2 -> k 1 k2) 
      else if n = 1 then k1 (fun k k3 -> k 1 k3) 
           else fibk 
                  (n - 1) 
                  (fun t4 -> k1 (fun k k5 -> 
                                  t4 
                                    (fun l k6 -> 
                                      fibk 
                                        (n - 2) 
                                        (fun t7 -> t7 
                                                     (fun r k8 -> k (l + r) k8) 
                                                     k6
                                                  )
                                      ) 
                                    k5
                                  ))
            
      in 
    fibk 
      5 
      (fun t9 -> t9 
                   (fun x k10 -> k10 x) 
                   (fun t11 -> let u = print t11 in let t = closure_count () 
                                                    in (fun x -> x) 0)
                )
    

  $ cat ../bench/fibk5.ml | ../../../back_rv64/RV64_compiler.exe -cps -call_arity -dcps -stop-after cps
  After CPS optimisations.
  let main =
    let rec fibk n e12 k1 = 
      if n = 0 then e12 1 k1 
      else if n = 1 then e12 1 k1 
           else fibk 
                  (n - 1) 
                  (fun l k6 -> fibk (n - 2) (fun r k8 -> e12 (l + r) k8) k6) 
                  k1
            
      in 
    fibk 
      5 
      (fun x k10 -> k10 x) 
      (fun t11 -> let u = print t11 in let t = closure_count () in (fun x -> x) 0)
    
