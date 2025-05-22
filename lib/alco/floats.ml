let e = epsilon_float

let others () =
  Alcotest.(check @@ float e) "0 is 0" 0. 0.;
  Alcotest.(check @@ float e) "0 is epsilon" 0. e;
  Alcotest.(check @@ neg @@ float e) "0 is not 1" 0. 1.;
  Alcotest.(check @@ neg @@ float e) "1 is not 0" 1. 0.;
  Alcotest.(check @@ float e) ".3 is .3" (0.1 +. 0.2) 0.3

let others_set = [ ("others", `Quick, others) ]
let () = Alcotest.run "Float tests" [ ("Other floats", others_set) ]
