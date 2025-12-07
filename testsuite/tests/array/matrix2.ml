(*
test
  (targets rv64 amd64)
  (run (stdout "rukaml_print_int 121500\n  rukaml_print_int 121500"))
*)

let arbitrary a = [|[|a; 2*a; 3*a|];[|3*a;a;2*a|]; [|2*a;a;3*a|]|]

let determinant_3x3 mtx =
  let column0 = get mtx 0 in
  let column1 = get mtx 1 in
  let column2 = get mtx 2 in
  ((get column0 0) * (get column1 1) * (get column2 2) +
   (get column0 1) * (get column1 2) * (get column2 0) +
   (get column0 2) * (get column1 0) * (get column2 1)) -
  ((get column0 2) * (get column1 1) * (get column2 0) +
   (get column0 1) * (get column1 0) * (get column2 2) +
   (get column0 0) * (get column1 2) * (get column2 1))

let mtx_mul_3x3 a b =
  let a0 = get a 0 in
  let a1 = get a 1 in
  let a2 = get a 2 in
  let b0 = get b 0 in
  let b1 = get b 1 in
  let b2 = get b 2 in
  [|[|(get a0 0) * (get b0 0) + (get a0 1) * (get b1 0) + (get a0 2) * (get b2 0);
      (get a0 0) * (get b0 1) + (get a0 1) * (get b1 1) + (get a0 2) * (get b2 1);
      (get a0 0) * (get b0 2) + (get a0 1) * (get b1 2) + (get a0 2) * (get b2 2)
    |];
    [|(get a1 0) * (get b0 0) + (get a1 1) * (get b1 0) + (get a1 2) * (get b2 0);
      (get a1 0) * (get b0 1) + (get a1 1) * (get b1 1) + (get a1 2) * (get b2 1);
      (get a1 0) * (get b0 2) + (get a1 1) * (get b1 2) + (get a1 2) * (get b2 2)
    |];
    [|
      (get a2 0) * (get b0 0) + (get a2 1) * (get b1 0) + (get a2 2) * (get b2 0);
      (get a2 0) * (get b0 1) + (get a2 1) * (get b1 1) + (get a2 2) * (get b2 1);
      (get a2 0) * (get b0 2) + (get a2 1) * (get b1 2) + (get a2 2) * (get b2 2)
    |]|]

let main =
  let a = arbitrary 3 in
  let b = arbitrary 5 in
  let ab = mtx_mul_3x3 a b in
  let deta = determinant_3x3 a in
  let detb = determinant_3x3 b in
  let detab = determinant_3x3 ab in
  let unit = print detab in
  print (deta * detb)

