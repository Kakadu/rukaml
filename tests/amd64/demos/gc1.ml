let main =
  let alive = (3,(2,(1,()))) in
  let tmp = gc_compact () in
  let tmp2 = print 42 in 
  0