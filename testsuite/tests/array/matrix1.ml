(*
   test
  (targets rv64 amd64)
  (run (stdout "rukaml_print_int 8"))
*)

let identity_mul_by_scalar a = [| [| a; 0; 0 |]; [| 0; a; 0 |]; [| 0; 0; a |] |]

let arbitrary a =
  [| [| a; 2 * a; 3 * a |]; [| 3 * a; a; 2 * a |]; [| 2 * a; a; 3 * a |] |]
;;

let determinant_3x3 mtx =
  let column0 = array_get mtx 0 in
  let column1 = array_get mtx 1 in
  let column2 = array_get mtx 2 in
  (array_get column0 0 * array_get column1 1 * array_get column2 2)
  + (array_get column0 1 * array_get column1 2 * array_get column2 0)
  + (array_get column0 2 * array_get column1 0 * array_get column2 1)
  - ((array_get column0 2 * array_get column1 1 * array_get column2 0)
     + (array_get column0 1 * array_get column1 0 * array_get column2 2)
     + (array_get column0 0 * array_get column1 2 * array_get column2 1))
;;

let main =
  let id = identity_mul_by_scalar 2 in
  let det = determinant_3x3 id in
  print det
;;
