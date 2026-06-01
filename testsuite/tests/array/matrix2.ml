(*
test
  (targets rv64 amd64)
  (run (stdout "rukaml_print_int 121500\n  rukaml_print_int 121500"))
*)

let arbitrary a = [|[|a; 2*a; 3*a|];[|3*a;a;2*a|]; [|2*a;a;3*a|]|]

let determinant_3x3 mtx =
  let column0 = array_get mtx 0 in
  let column1 = array_get mtx 1 in
  let column2 = array_get mtx 2 in
  ((array_get column0 0) * (array_get column1 1) * (array_get column2 2) +
   (array_get column0 1) * (array_get column1 2) * (array_get column2 0) +
   (array_get column0 2) * (array_get column1 0) * (array_get column2 1)) -
  ((array_get column0 2) * (array_get column1 1) * (array_get column2 0) +
   (array_get column0 1) * (array_get column1 0) * (array_get column2 2) +
   (array_get column0 0) * (array_get column1 2) * (array_get column2 1))

let mtx_mul_3x3 a b =
  let a0 = array_get a 0 in
  let a1 = array_get a 1 in
  let a2 = array_get a 2 in
  let b0 = array_get b 0 in
  let b1 = array_get b 1 in
  let b2 = array_get b 2 in
  [|[|(array_get a0 0) * (array_get b0 0) + (array_get a0 1) * (array_get b1 0) + (array_get a0 2) * (array_get b2 0);
      (array_get a0 0) * (array_get b0 1) + (array_get a0 1) * (array_get b1 1) + (array_get a0 2) * (array_get b2 1);
      (array_get a0 0) * (array_get b0 2) + (array_get a0 1) * (array_get b1 2) + (array_get a0 2) * (array_get b2 2)
    |];
    [|(array_get a1 0) * (array_get b0 0) + (array_get a1 1) * (array_get b1 0) + (array_get a1 2) * (array_get b2 0);
      (array_get a1 0) * (array_get b0 1) + (array_get a1 1) * (array_get b1 1) + (array_get a1 2) * (array_get b2 1);
      (array_get a1 0) * (array_get b0 2) + (array_get a1 1) * (array_get b1 2) + (array_get a1 2) * (array_get b2 2)
    |];
    [|
      (array_get a2 0) * (array_get b0 0) + (array_get a2 1) * (array_get b1 0) + (array_get a2 2) * (array_get b2 0);
      (array_get a2 0) * (array_get b0 1) + (array_get a2 1) * (array_get b1 1) + (array_get a2 2) * (array_get b2 1);
      (array_get a2 0) * (array_get b0 2) + (array_get a2 1) * (array_get b1 2) + (array_get a2 2) * (array_get b2 2)
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

