type cfg =
  { mutable out : [ `Stdout | `File of string ]
  ; mutable input_file : string option
  }

open Miniml

module ToLLVM = struct
  let codegen typedtree fout =
    Format.printf "%a\n%!" Typedtree.pp_hum typedtree;
    Result.ok ()
  ;;

  let run cfg =
    let text =
      let filename = cfg.input_file |> Option.get in
      Stdio.In_channel.with_file filename ~f:Stdio.In_channel.input_all
    in
    let promote_error r =
      Result.map_error (fun x -> (x :> [ Parsing.error | Inferencer.error ])) r
    in
    let ( let* ) x f = Result.bind x f in
    (* let ( let+ ) x f = Result.map f x in *)
    let* ast = Miniml.Parsing.parse text |> promote_error in
    let* typedtree = Inferencer.w ast |> promote_error in
    let typedtree = Typedtree.compact_expr typedtree in
    codegen typedtree cfg.out |> promote_error
  ;;
end

let cfg = { out = `Stdout; input_file = None }

let print_errors = function
  | #Miniml.Parsing.error as e -> Format.printf "%a\n%!" Parsing.pp_error e
  | #Miniml.Inferencer.error as e -> Format.printf "%a\n%!" Inferencer.pp_error e
;;

let () =
  Arg.parse
    [ ( "-o"
      , Arg.String
          (function
           | "-" -> cfg.out <- `Stdout
           | s -> cfg.out <- `File s)
      , "" )
    ]
    (fun s -> cfg.input_file <- Some s)
    "help";
  match ToLLVM.run cfg with
  | Result.Ok () -> ()
  | Error err -> print_errors err
;;
