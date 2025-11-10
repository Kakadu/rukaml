open Base
module Format = Stdlib.Format
open Format
open Frontend

let run_structure text =
  match Parsing.parse_structure text with
  | Error s -> Format.printf "parsing error: %a\n" Parsing.pp_error s
  | Result.Ok ast ->
    (match Inferencer.structure Typedtree.empty_table ast with
     | Result.Error e -> Format.printf "inferencer error: %a\n" Inferencer.pp_error e
     | Result.Ok stru ->
       Format.printf "result:%!";
       Format.printf "@[<v>@ ";
       Format.printf "@[%a@]@ " Pprinttyped.pp_stru stru;
       Format.printf "@]\n%!");
    ()
;;

let () = Stdio.In_channel.(input_all stdin) |> String.rstrip |> run_structure
