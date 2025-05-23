open Base
module Format = Stdlib.Format
open Format
open Frontend

let run_single parser pp pp_typed pp_err text =
  let ( let* ) x f = Result.bind x ~f in
  (let* stru = parser text in
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
   let* stru_typed = Inferencer.structure stru in
   Format.printf "After CCovv.\n%!";
   Format.printf "%a\n%!" pp_typed stru_typed;
   let anf = Compile_lib.ANF.(anf_stru stru_typed |> simplify_stru) in
   Format.printf "After ANF transformation.\n%!";
   Format.printf "%a\n%!" Compile_lib.ANF.pp_stru anf;
   Result.return ())
  |> function
  | Error s -> Format.printf "Error: %a\n%!" pp_err s
  | Result.Ok () -> ()
;;

type mode = Stru

[@@@ocaml.warning "-69"]

type opts =
  { mutable batch : bool
  ; mutable mode : mode
  ; mutable log_parsing : bool
  ; mutable log_cc : bool
  ; mutable log_anf : bool
  }

let () =
  let opts =
    { batch = false; mode = Stru; log_cc = false; log_parsing = false; log_anf = false }
  in
  Stdlib.Arg.parse
    [ "-", Stdlib.Arg.Unit (fun () -> opts.batch <- true), " read from stdin"
    ; "-vcc", Stdlib.Arg.Unit (fun () -> opts.log_cc <- true), " verbose logging of CC"
    ; ( "-vanf"
      , Stdlib.Arg.Unit (fun () -> opts.log_anf <- true)
      , " verbose logging of ANF transformation" )
    ; ( "-vp"
      , Stdlib.Arg.Unit (fun () -> opts.log_parsing <- true)
      , " verbose logging of parsing" )
    ]
    (fun _ -> assert false)
    "TODO";
  Parsing.set_logging opts.log_parsing;
  CConv.set_logging opts.log_cc;
  Compile_lib.ANF.set_logging opts.log_cc;
  let s = Stdio.In_channel.(input_all stdin) |> String.rstrip in
  let on_error ppf = function
    | #Inferencer.error as e -> Inferencer.pp_error ppf e
    | #Parsing.error as e -> Parsing.pp_error ppf e
  in
  (match opts.mode with
   | Stru ->
     run_single Parsing.parse_structure Pprint.structure Pprinttyped.pp_stru on_error)
    s
;;
