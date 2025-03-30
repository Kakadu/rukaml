(* load all the available plugins *)
let () = Sites.Plugins.Plugins.load_all ()

module Config = struct
  let dump_parsetree = ref false
  let dump_typedtree = ref false
  let out_file = ref "a.out"

  type input_from =
    | Stdin
    | File of string

  let input = ref Stdin
end

let default_args =
  [ "-dparsetree", Arg.Unit (fun () -> Config.dump_parsetree := true), " dump parsetree"
  ; "-dtypedtree", Arg.Unit (fun () -> Config.dump_typedtree := true), " dump typedtree"
  ; "-", Arg.Unit (fun () -> Config.input := Config.Stdin), "read from stdin"
  ]
;;

let read_input = function
  | Config.Stdin -> Result.Ok In_channel.(input_all stdin)
  | File name ->
    let ch = In_channel.open_text name in
    let str = In_channel.input_all ch in
    In_channel.close ch;
    Result.Ok str
;;

open Frontend

let ( let* ) = Result.bind

type backend_error =
  | Backend_error : 'a * (Format.formatter -> 'a -> unit) -> backend_error

let apply_backend parsed typed ~filename =
  let* (module Back) = Registration.backend_exn () in
  Result.map_error
    (fun e -> `Backend_error (Backend_error (e, Back.pp_error)))
    (Back.run parsed typed ~filename)
;;

let () =
  let args = default_args @ !Registration.extra_cmd_switches in
  Arg.parse args (fun s -> Config.input := Config.File s) " MiniML compiler";
  match
    let* input = read_input !Config.input in
    let* parsed = Parsing.parse_structure input in
    let () =
      if !Config.dump_parsetree then Format.printf "%a\n%!" Frontend.Pprint.structure parsed
    in
    let* typed = Inferencer.(structure ~env:start_env) parsed in
    let () =
      if !Config.dump_typedtree
      then Format.printf "%a\n%!" Frontend.Pprinttyped.pp_stru typed
    in
    apply_backend parsed typed ~filename:!Config.out_file
  with
  | Result.Ok () -> ()
  | Error (#Parsing.error as e) -> Format.eprintf "%a\n%!" Parsing.pp_error e
  | Error (#Inferencer.error as e) -> Format.eprintf "%a\n%!" Inferencer.pp_error e
  | Error `Backend_unspecified ->
    Format.eprintf "No backend selected.";
    exit 1
  | Error (`Backend_error (Backend_error (e, pp))) ->
    Format.eprintf "%a\n%!" pp e;
    exit 1
;;
