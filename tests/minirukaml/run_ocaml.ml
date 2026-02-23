open Stdlib.Format

type mode =
  | Patt
  | Expr
  | Stru

type target =
  | Parsetree
  | Typedtree

type opts =
  { mutable mode : mode
  ; mutable target : target
  ; mutable in_path : string option
  ; mutable out_path : string option
  }

let run_expr ~out opts input =
  let open Minirukaml in
  match parse_expression input with
  | Error err -> fprintf out "parsing error: %a" pp_parsing_error err
  | Ok ast ->
    (match opts.target with
     | Parsetree -> fprintf out "parsed: %a" pp_expression ast
     | Typedtree -> failwith "not implemented")
;;

let () =
  let opts = { mode = Stru; target = Parsetree; in_path = None; out_path = None } in
  let mode_arg v = Stdlib.Arg.Unit (fun () -> opts.mode <- v) in
  let target_arg v = Stdlib.Arg.Unit (fun () -> opts.target <- v) in
  Stdlib.Arg.parse
    [ ("-expr", mode_arg Expr, " expression")
    ; ("-stru", mode_arg Stru, " structure")
    ; ("-parse", target_arg Parsetree, " parsing")
    ; ("-infer", target_arg Typedtree, " typing")
    ; ("-o", Stdlib.Arg.String (fun s -> opts.out_path <- Some s), " output file")
    ]
    (fun arg ->
       match opts.in_path with
       | None -> opts.in_path <- Some arg
       | Some _ ->
         printf "unexpected arg";
         exit 1)
    "Usage: minirukaml.exe source.ml";
  let input =
    (match opts.in_path with
     | None -> Stdio.In_channel.input_all stdin
     | Some path -> Stdio.In_channel.read_all path)
    |> Base.String.rstrip
  in
  let out_channel =
    match opts.out_path with
    | None -> stdout
    | Some path -> open_out path
  in
  let run =
    match opts.mode with
    | Expr -> run_expr
    | Patt | Stru -> failwith "not implemented"
  in
  run ~out:out_channel opts input;
  Stdio.Out_channel.flush out_channel;
  Stdio.Out_channel.close_no_err out_channel
;;
