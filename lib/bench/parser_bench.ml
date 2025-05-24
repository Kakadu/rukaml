open Benchmark

let str1 =
  let b = Buffer.create 10 in
  for i = 0 to 100 do
    if i <> 0 then Buffer.add_char b '+';
    Buffer.add_string b "asdf"
  done;
  Buffer.contents b

let run parser str = assert (Option.is_some (parser str))

let () =
  print_newline ();

  (* let's exercise the *1 functions: *)
  (* let _ = latency1 ~name:"int-1-lat" 1000L f_int 10000 in
     let s = throughput1 ~name:"int-1-thru" 5 f_int 10000 in
     print_gc s;
     print_newline (); *)

  (* now let's exercise the *N functions: *)
  (* let res =
       throughputN ~repeat:5 10
         [
           ("int", f_int, 10000);
           ("int32", f_int32, 10000);
           ("int64", f_int64, 10000);
         ]
     in
     print_newline ();
     tabulate res;
     print_gc res; *)
  print_newline ();
  let res =
    latencyN 20000L
      [
        (* ("int", f_int, 10000);
           ("int32", f_int32, 10000);
           ("int64", f_int64, 10000); *)
        ("recdesc", run Parser.parse_expr_string, str1);
        ("C", run ParseSIMD.parse_expr_string, str1);
      ]
  in
  print_newline ();
  tabulate res
