
let test () =
  let scru = (1, "one") in
  let (temp8, temp9) = scru in
  (if (temp9 = "two")
    then output_string stdout "equal"
    else output_string stdout "not equal")
  (* match scru with
  (* | _, 0, _, _ -> printf "test failed 1"
  | _, _, false, _ -> printf "test failed 2" *)
  | _,   "two" -> output_string stdout "test failed 3"
  (* | (), 1, true, "one" -> printf "test passed"
  | (), 1, true, _ -> printf "test failed 4"
  | (), 1, _, _ -> printf "test failed 5"
  | (), _, _, _ -> printf "test failed 6"
  | _, _, _, _ -> printf "test failed 7" *)
  | _ -> output_string stdout "test failed 8" *)
;;

let main =
  let t = test () in
  0
;;
