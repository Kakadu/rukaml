open Base
module Format = Caml.Format
open Format
open Miniml

let run_single parser pp pp_err text =
  let ast = Angstrom.parse_string ~consume:All parser text in
  match ast with
  | Error s -> Format.printf "Error: %a\n%!" pp_err s
  | Result.Ok stru ->
    Format.printf "Parsed: %a\n%!" pp stru;
    let stru = List.concat_map ~f:CConv.conv stru in
    Format.printf "After CCovv.\n%!";
    Format.printf "%a\n%!" pp stru;
    ()
;;

type mode = Stru

type opts =
  { mutable batch : bool
  ; mutable mode : mode
  ; mutable log : bool
  }

let () =
  let opts = { batch = false; mode = Stru; log = false } in
  let mode_arg v = Caml.Arg.Unit (fun () -> opts.mode <- v) in
  Caml.Arg.parse
    [ "-", Caml.Arg.Unit (fun () -> opts.batch <- true), " read from stdin"
    ; "-v", Caml.Arg.Unit (fun () -> opts.log <- true), " verbose logging"
    ; "-stru", mode_arg Stru, " structure"
    ]
    (fun _ -> assert false)
    "TODO";
  Parsing.set_logging opts.log;
  let s = Stdio.In_channel.(input_all stdin) |> String.rstrip in
  (* Format.printf "%S\n%!" s; *)
  (match opts.mode with
   | Stru -> run_single Parsing.structure Pprint.structure Format.pp_print_string)
    s
;;
