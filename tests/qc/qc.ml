open Qc_lib

let generate n =
  for _ = 1 to n do
    Format.(printf "%a\n%!" Frontend.Pprint.pp_pattern)
      (QCheck.Gen.generate1 Qc_lib.(gen_pattern_sized 3))
  done
;;

let generate_expr n =
  for _ = 1 to n do
    Format.(printf "%a\n%!" Frontend.Pprint.pp_expr)
      (QCheck.Gen.generate1 Qc_lib.(gen_expr_sized 3))
  done
;;

let () =
  QCheck_runner.set_verbose true;
  Arg.parse
    [ "-seed", Arg.Int QCheck_runner.set_seed, " Set seed"
    ; "-stop", Arg.Unit (fun _ -> exit 0), " Exit"
    ; "-gen", Arg.Int generate, "  "
    ; "-gen-expr", Arg.Int generate_expr, "  "
    ; ( "-pattern"
      , Arg.Unit
          (fun _ ->
            let _ = run_pattern () in
            ())
      , " " )
    ; ( "-expr"
      , Arg.Unit
          (fun _ ->
            let _ = run_expr () in
            ())
      , " " )
    ]
    (fun _ -> assert false)
    "help"
;;
