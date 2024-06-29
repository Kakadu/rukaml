open Qc_lib
open Qc_lib.Qc_parser
open AST

let make_print_parse ?name gen ~of_string ~to_string eq =
  QCheck.Test.make ?name gen (fun c1 ->
      match to_string c1 with
      | exception exc1 ->
          Printf.printf "ERROR: %s %s %d\n%!"
            (Stdlib.Printexc.to_string exc1)
            __FILE__ __LINE__;
          false
      | j -> (
          match of_string j with
          | c2 -> eq c1 c2
          | exception exc1 ->
              Printf.printf "ERROR while parsing string %S: %s %s %d\n%!" j
                (Stdlib.Printexc.to_string exc1)
                __FILE__ __LINE__;
              print_endline (Printexc.get_backtrace ());
              false))

let print_parse_is_identity_identifier =
  make_print_parse arbitrary_identifier
    ~of_string:(fun x -> Option.get (Parser.parse_ident_string x))
    ~to_string:Pprint.show_ident Stdlib.( = )

let print_parse_is_identity_expression =
  make_print_parse arbitrary_expression
    ~of_string:(fun x -> Option.get (Parser.parse_expr_string x))
    ~to_string:Pprint.show_expr Stdlib.( = )

let print_parse_is_identity_stmt =
  make_print_parse ~name:"print/parse stmt" arbitrary_stmt
    ~of_string:(fun x -> Option.get (Parser.parse_stmt_string x))
    ~to_string:Pprint.show_stmt Stdlib.( = )

(* type cfg = {
     mutable cfg_expr : bool;
     mutable cfg_stmt : bool; (* mutable cfg_fun_m : bool; *)
   }

   let cfg = { cfg_expr = false; cfg_stmt = false (* cfg_fun_m = true  *) } *)

let () =
  (* Arg.parse
     [
       ("-expr", Arg.Unit (fun () -> cfg.cfg_expr <- true), "");
       ("-stmt", Arg.Unit (fun () -> cfg.cfg_stmt <- true), "");
       (* ("-fun-m", Arg.Unit (fun () -> cfg.cfg_fun_m <- true), ""); *)
     ]
     (fun _ -> assert false)
     ""; *)
  Format.printf "Failed ident tests: %d\n%!"
  @@ QCheck_base_runner.run_tests [ print_parse_is_identity_identifier ];
  Format.printf "Failed expr tests: %d\n%!"
  @@ QCheck_base_runner.run_tests [ print_parse_is_identity_expression ];
  Format.printf "Failed stmt tests: %d\n%!"
  @@ QCheck_base_runner.run_tests [ print_parse_is_identity_stmt ];
  (* if cfg.cfg_rule then
       Format.printf "Failed rule tests: %d\n%!"
       @@ QCheck_base_runner.run_tests [ print_parse_is_identity_rule ];
     if cfg.cfg_cond then
       Format.printf "Failed condition tests: %d\n%!"
       @@ QCheck_base_runner.run_tests [ print_parse_is_identity_condition ]; *)
  ()
