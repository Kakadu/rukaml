open Base
module Format = Caml.Format
open Format
open Miniml

let run_single parser pp pp_err text =
  let ast = Angstrom.parse_string ~consume:All parser text in
  match ast with
  | Error s -> printf "Error: %a\n%!" pp_err s
  | Result.Ok ast -> printf "Parsed.@,%a%!" pp ast
;;

type mode =
  | ELong
  | Eprio
  | E
  | VB
  | Stru

type opts =
  { mutable batch : bool
  ; mutable mode : mode
  ; mutable log : bool
  }

let () =
  let opts = { batch = false; mode = VB; log = false } in
  Caml.Arg.parse
    [ "-", Caml.Arg.Unit (fun () -> opts.batch <- true), " read from stdin"
    ; "-long", Caml.Arg.Unit (fun () -> opts.mode <- ELong), " long"
    ; "-prio", Caml.Arg.Unit (fun () -> opts.mode <- Eprio), " prio"
    ; "-e", Caml.Arg.Unit (fun () -> opts.mode <- E), " basic expr"
    ; "-vb", Caml.Arg.Unit (fun () -> opts.mode <- VB), " value binding"
    ; "-stru", Caml.Arg.Unit (fun () -> opts.mode <- Stru), " structure"
    ; "-v", Caml.Arg.Unit (fun () -> opts.log <- true), " verbose logging"
    ]
    (fun _ -> assert false)
    "TODO";
  Parsing.set_logging opts.log;
  let s = Stdio.In_channel.(input_all stdin) |> String.rstrip in
  Format.printf "%S\n%!" s;
  (match opts.mode with
   | ELong -> run_single Parsing.(pack.expr_long pack) Pprint.pp_expr pp_print_string
   | E -> run_single Parsing.(pack.expr pack) Pprint.pp_expr pp_print_string
   | Eprio -> run_single Parsing.(pack.prio pack) Pprint.pp_expr pp_print_string
   | VB -> run_single Parsing.value_binding Pprint.pp_value_binding pp_print_string
   | Stru ->
     run_single
       Parsing.structure
       (fun ppf stru ->
         match Inferencer.structure stru with
         | Result.Ok ty -> fprintf ppf "@[%a@]@ " Typedtree.pp_stru ty
         | Result.Error e -> fprintf ppf "Error: %a" Inferencer.pp_error e)
       pp_print_string)
    s
;;
