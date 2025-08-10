  $ cat ../bench/fib0.ml | ../../../back_rv64/RV64_compiler.exe -cps -dcps -stop-after cps
  After CPS optimisations.
  let main =
    let rec fibk n k1 = 
      k1 (fun k k2 -> 
           if n = 0 then k 1 k2 
           else if n = 1 then k 1 k2 
                else fibk 
                       (n - 1) 
                       (fun t3 -> t3 
                                    (fun l k4 -> 
                                      fibk 
                                        (n - 2) 
                                        (fun t5 -> t5 
                                                     (fun r k6 -> k (l + r) k6) 
                                                     k4
                                                  )
                                      ) 
                                    k2
                                 )
                ) 
      in 
    fibk 
      5 
      (fun t7 -> t7 
                   (fun x k8 -> k8 x) 
                   (fun t9 -> let u = print t9 in let t = closure_count () in 
                                                  (fun x -> x) 0)
                )
    

  $ cat ../bench/fib0.ml | ../../../back_rv64/RV64_compiler.exe -cps -call_arity -dcps -stop-after cps
  After CPS optimisations.
  let main =
    let rec fibk n e10 k1 = 
      if n = 0 then e10 1 k1 
      else if n = 1 then e10 1 k1 
           else fibk 
                  (n - 1) 
                  (fun l k4 -> fibk (n - 2) (fun r k6 -> e10 (l + r) k6) k4) 
                  k1
            
      in 
    fibk 
      5 
      (fun x k8 -> k8 x) 
      (fun t9 -> let u = print t9 in let t = closure_count () in (fun x -> x) 0)
    
