open Base
module Format = Stdlib.Format
open Format
open Frontend

let run_single parser pp pp_err text =
  let ast = Angstrom.parse_string ~consume:All parser text in
  match ast with
  | Error s -> printf "Error: %a\n%!" pp_err s
  | Result.Ok ast -> printf "@[<v>Parsed.@ @[%a@]@]" pp ast
;;

type mode =
  | ELong
  | Eprio
  | E
  | VB
  | Stru

[@@@ocaml.warning "-69"]

type opts =
  { mutable batch : bool
  ; mutable mode : mode
  ; mutable log : bool
  }

let () =
  let opts = { batch = false; mode = VB; log = false } in
  Stdlib.Arg.parse
    [ "-", Stdlib.Arg.Unit (fun () -> opts.batch <- true), " read from stdin"
    ; "-long", Stdlib.Arg.Unit (fun () -> opts.mode <- ELong), " long"
    ; "-prio", Stdlib.Arg.Unit (fun () -> opts.mode <- Eprio), " prio"
    ; "-e", Stdlib.Arg.Unit (fun () -> opts.mode <- E), " basic expr"
    ; "-vb", Stdlib.Arg.Unit (fun () -> opts.mode <- VB), " value binding"
    ; "-stru", Stdlib.Arg.Unit (fun () -> opts.mode <- Stru), " structure"
    ; "-v", Stdlib.Arg.Unit (fun () -> opts.log <- true), " verbose logging"
    ]
    (fun _ -> assert false)
    "TODO";
  Parsing.set_logging opts.log;
  Parsing.set_logging false;
  let s = Stdio.In_channel.(input_all stdin) |> String.rstrip in
  (* Format.printf "%S\n%!" s; *)
  (match opts.mode with
   | ELong -> run_single Parsing.(pack.expr_long pack) Pprint.pp_expr pp_print_string
   | E -> run_single Parsing.(pack.expr pack) Pprint.pp_expr pp_print_string
   | Eprio -> run_single Parsing.(pack.prio pack) Pprint.pp_expr pp_print_string
   | VB -> run_single Parsing.value_binding Pprint.pp_value_binding pp_print_string
   | Stru ->
     run_single
       Parsing.structure
       (fun ppf stru ->
          Format.fprintf ppf "@[<v>@[%a@]@," Pprint.structure stru;
          (* Format.printf "Parsed: %S\n%!" (Parsetree.show_structure stru); *)
          match Inferencer.structure Typedtree.empty_table stru with
          | Result.Ok ty -> fprintf ppf "@[%a@]@ @]" Pprinttyped.pp_stru ty
          | Result.Error e -> fprintf ppf "Error: %a@]" Inferencer.pp_error e)
       pp_print_string)
    s
;;
