(*
test
  (targets rv64 amd64)
  (run
    (stdout
      "rukaml_print_int 0"
      "rukaml_print_int 1"
      "rukaml_print_int 2"
      "rukaml_print_int 3"
      "rukaml_print_int 4"
      "rukaml_print_int 5"
      "rukaml_print_int 6"
      "rukaml_print_int 7"
      "rukaml_print_int 8"
      "rukaml_print_int 9"
      "rukaml_print_int 10"
      "rukaml_print_int 53"
      "rukaml_print_int 1"))
*)

let rec fac n u =
  if n=1 then 1 else fac (n-1) u *  n

let rec loop n j =
  let t = print n in
  if n=10
  then (let v = print 53 in 1)
  else ( loop (n+1) j)


let main =
  let t44 = loop 0 12 in
  let tmppppppp = print t44 in
  0
