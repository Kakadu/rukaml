open Base
module Format = Stdlib.Format
open Stdio
open Miniml

let run_single text =
  print_endline "compiler";
  let ( let* ) x f = Result.bind x ~f in
  let* ast = Miniml.Parsing.parse text in
  let* typedtree = Inferencer.w ast in
  Format.set_margin 1000;
  Format.set_max_indent 100;
  (* Format.printf "%a\n%!" Typedtree.pp_expr typedtree; *)
  Compile_lib.Compile.compile typedtree;
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
