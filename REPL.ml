open Base
module Format = Caml.Format
open Format
open Miniml

let run_single text =
  let ast = Parsing.parse_structure text in
  match ast with
  | Error s -> Format.printf "Error: %a\n%!" Parsing.pp_error s
  | Result.Ok ast ->
    Format.printf "Parsed: %a\n%!" Pprint.pp_stru ast;
    (match Inferencer.structure ast with
     | Result.Ok ty ->
       Format.printf "Result:%!";
       Format.printf "@[<v>@ ";
       List.iter ~f:(Format.printf "@[%a@]@ " Typedtree.pp_vb_hum) ty;
       Format.printf "@]\n%!"
     (* Format.printf "%a\n%!" Typedtree.pp_expr ty *)
     | Result.Error e -> Format.printf "Error: %a" Inferencer.pp_error e)
;;

let run_repl =
  let rec read_until_end acc =
    let s = Stdio.In_channel.(input_line_exn stdin) in
    if String.is_suffix s ~suffix:";;"
    then sprintf "%s\n%s" acc (String.drop_suffix s 2)
    else read_until_end (sprintf "%s\n%s" acc (Stdlib.String.trim s))
  in
  let rec loop () =
    Format.printf "# %!";
    let line = read_until_end "" in
    if String.equal line "#quit"
    then ()
    else (
      run_single line;
      loop ())
  in
  loop
;;

type opts = { mutable batch : bool }

let () =
  let opts = { batch = false } in
  Caml.Arg.parse
    [ "-", Caml.Arg.Unit (fun () -> opts.batch <- true), "read from stdin" ]
    (fun _ -> assert false)
    "TODO";
  (if opts.batch
  then fun () -> Stdio.In_channel.(input_all stdin) |> String.rstrip |> run_single
  else run_repl)
    ()
;;
