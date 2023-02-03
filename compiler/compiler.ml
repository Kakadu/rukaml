open Base
module Format = Stdlib.Format
open Stdio
open Miniml

let run_single text =
  Format.set_margin 1000;
  Format.set_max_indent 100;
  let ( let* ) x f = Result.bind x ~f in
  let* stru = Miniml.Parsing.parse_structure text in
  (* List.iter ~f:(Format.printf "%a\n%!" Pprint.pp_value_binding) stru; *)
  let stru = List.concat_map ~f:Compile_lib.Compile.conv stru in
  (* List.iter ~f:(Format.printf "%a\n%!" Pprint.pp_value_binding) stru; *)
  let* xs = Result.all (List.map ~f:Inferencer.vb stru) in
  List.iter ~f:(Format.printf "%a\n%!" Typedtree.pp_vb_hum) xs;
  Result.return ()
;;

let print_errors = function
  | #Parsing.error as e -> Format.printf "%a\n%!" Parsing.pp_error e
  | #Inferencer.error as e -> Format.printf "%a\n%!" Inferencer.pp_error e
;;

let () =
  match Stdio.In_channel.(input_all stdin) |> String.rstrip |> run_single with
  | Result.Error e -> print_errors e
  | Ok () -> ()
;;
