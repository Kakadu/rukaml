open Base
module Format = Stdlib.Format
open Format
open Frontend

let run_single parser pp pp_err text =
  let ast = Angstrom.parse_string ~consume:All parser text in
  match ast with
  | Error s -> Format.printf "Error: %a\n%!" pp_err s
  | Result.Ok stru ->
    Format.printf "Parsed: %a\n%!" pp stru;
    let stru =
      let init = CConv.standart_globals, [] in
      Stdlib.ListLabels.fold_left
        (stru : Parsetree.structure)
        ~init
        ~f:(fun (glob, ans) stru ->
          let new_strus = CConv.conv ~standart_globals:glob stru in
          let new_glob =
            List.fold_left ~init:glob new_strus ~f:(fun acc -> function
              | _, PVar s, _ -> CConv.String_set.add s acc
              | _, PTuple _, _ -> acc)
          in
          new_glob, List.append ans new_strus)
      |> snd
    in
    Format.printf "After CCovv.\n%!";
    Format.printf "%a\n%!" pp stru;
    ()
;;

type mode = Stru

[@@@ocaml.warning "-69"]

type opts =
  { mutable batch : bool
  ; mutable mode : mode
  ; mutable log_cc : bool
  ; mutable log_parsing : bool
  }

let () =
  let opts = { batch = false; mode = Stru; log_cc = false; log_parsing = false } in
  Stdlib.Arg.parse
    [ "-", Stdlib.Arg.Unit (fun () -> opts.batch <- true), " read from stdin"
    ; "-vcc", Stdlib.Arg.Unit (fun () -> opts.log_cc <- true), " verbose logging of CC"
    ; ( "-vp"
      , Stdlib.Arg.Unit (fun () -> opts.log_parsing <- true)
      , " verbose logging of parsing" )
    ]
    (fun _ -> assert false)
    "TODO";
  Parsing.set_logging opts.log_parsing;
  CConv.set_logging opts.log_cc;
  let s = Stdio.In_channel.(input_all stdin) |> String.rstrip in
  (match opts.mode with
   | Stru -> run_single Parsing.structure Pprint.structure Format.pp_print_string)
    s
;;
