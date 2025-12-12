open Base
module Format = Stdlib.Format
open Format
open Frontend

let run_single parser pp pp_err text =
  let ast = Angstrom.parse_string ~consume:All parser text in
  match ast with
  | Error s -> Format.printf "Error: %a\n%!" pp_err s
  | Result.Ok ast ->
    Format.printf "Parsed: %a\n%!" pp ast;
    (* (match Inferencer.w ast with
     | Result.Ok ty ->
       Format.printf "Result:%!";
       Format.printf "@[<v>@ ";
       Format.printf "@[%a@]@ " Typedtree.pp_expr ty;
       Format.printf "@]\n%!"
     (* Format.printf "%a\n%!" Typedtree.pp_expr ty *)
     | Result.Error e -> Format.printf "Error: %a" Inferencer.pp_error e); *)
    ()
;;

type mode =
  | ELong
  | Eprio
  | E
  | VB
  | Stru
  | Pat
  | CoreType

[@@@ocaml.warning "-69"]

type opts =
  { mutable mode : mode
  ; mutable log : bool
  ; mutable batch : bool
  }

let () =
  let opts = { batch = false; mode = VB; log = false } in
  let mode_arg v = Stdlib.Arg.Unit (fun () -> opts.mode <- v) in
  Stdlib.Arg.parse
    [ "-", Stdlib.Arg.Unit (fun () -> opts.batch <- true), " read from stdin"
    ; "-v", Stdlib.Arg.Unit (fun () -> opts.log <- true), " verbose logging"
    ; "-long", mode_arg ELong, " long"
    ; "-prio", mode_arg Eprio, " prio"
    ; "-e", mode_arg E, " basic expr"
    ; "-vb", mode_arg VB, " value binding"
    ; "-stru", mode_arg Stru, " structure"
    ; "-pat", mode_arg Pat, " pattern"
    ; "-core-type", mode_arg CoreType, " core type"
    ]
    (fun _ -> assert false)
    "TODO";
  Parsing.set_logging opts.log;
  let s = Stdio.In_channel.(input_all stdin) |> String.rstrip in
  (match opts.mode with
   | ELong ->
     run_single Parsing.(pack.expr_long pack) Pprint.pp_expr Format.pp_print_string
   | E -> run_single Parsing.(pack.expr pack) Pprint.pp_expr Format.pp_print_string
   | Eprio -> run_single Parsing.(pack.prio pack) Pprint.pp_expr Format.pp_print_string
   | VB ->
     run_single Parsing.(value_binding) Pprint.pp_value_binding Format.pp_print_string
   | Pat -> run_single Parsing.pattern Pprint.pp_pattern pp_print_string
   | CoreType -> run_single Parsing.core_type Pprint.pp_core_type Format.pp_print_string
   | Stru -> run_single Parsing.structure Pprint.structure Format.pp_print_string)
    s
;;
