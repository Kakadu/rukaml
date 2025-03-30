let fst p =
  let (a, b) = p in
  a

let snd p =
  let (a, b) = p in
  b

let fac3 n =
  let store = (3, (2, (1, ()))) in
  let n3 = fst store in
  let n2 = fst (snd store) in
  let n1 = fst (snd (snd store)) in
  n3 * n2 * n1

let main =
  let alive = (3, (2, (1, ()))) in
  let tmp1 = fac3 0 in
  let tmp2 = gc_compact () in
  let tmp4 = gc_stats () in
  let tmpl = print 42 in
  0
