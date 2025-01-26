type cfg = {
  mutable out_file : string;
  mutable stdin_file : bool;  (** Only source code goes here *)
  mutable input_files : string list;
      (** Interface .rkmi and source .ml files *)
  mutable compile_only : bool;  (** If false, do not generate call to 'main' *)
}

let cfg_empty () =
  {
    out_file = "a.out";
    stdin_file = false;
    input_files = [];
    compile_only = false;
  }

let add_input_file cfg s = cfg.input_files <- cfg.input_files @ [ s ]

open Miniml

let exit_because fmt =
  Format.kasprintf
    (fun s ->
      Printf.eprintf "%s\n%!" s;
      exit 1)
    fmt

let frontend cfg =
  let rkmis, the_module_file, text =
    let rec loop rkmis file = function
      | [] -> (List.rev rkmis, file)
      | h :: tl when String.ends_with ~suffix:".rkmi" h ->
          loop (h :: rkmis) file tl
      | h :: tl when String.ends_with ~suffix:".ml" h -> (
          match file with
          | Some oldfile ->
              Printf.eprintf "You can't specify two .mls: %s and %s" oldfile h;
              exit 1
          | None -> loop rkmis (Some h) tl)
      | h :: _ ->
          Printf.eprintf "Don't know what to do with %s" h;
          exit 1
    in
    let rkmis, file = loop [] None cfg.input_files in
    let rkmis = Compile_lib.Rkmi.(to_repr @@ List.map from_file rkmis) in
    match (file, cfg.stdin_file) with
    | Some file, true -> exit_because "Can't use both stdin and file %s\n" file
    | None, false -> exit_because "Program is not specified"
    | None, true -> (rkmis, "module.ml", In_channel.input_all In_channel.stdin)
    | Some file, false ->
        (rkmis, file, In_channel.with_open_text file In_channel.input_all)
  in

  let promote_error r =
    Result.map_error (fun x -> (x :> [ Parsing.error | Inferencer.error ])) r
  in
  let ( let* ) x f = Result.bind x f in
  (* let ( let+ ) x f = Result.map f x in *)
  let* stru = Miniml.Parsing.parse_structure text |> promote_error in
  let stru =
    let init = (CConv.standart_globals, []) in
    Stdlib.ListLabels.fold_left
      (stru : Parsetree.structure)
      ~init
      ~f:(fun (glob, ans) stru ->
        let new_strus = CConv.conv ~standart_globals:glob stru in
        let new_glob =
          ListLabels.fold_left ~init:glob new_strus ~f:(fun acc -> function
            | _, Parsetree.PVar s, _ -> CConv.String_set.add s acc
            | _, PTuple _, _ -> acc)
        in
        (new_glob, List.append ans new_strus))
    |> snd
  in
  let* typedtree =
    let env =
      Compile_lib.Rkmi.fold
        (fun _ { Compile_lib.Rkmi.name; typ; _ } acc ->
          Inferencer.Type_env.extend_string name typ acc)
        Inferencer.start_env rkmis
    in

    Inferencer.structure ~env stru
    |> Result.map_error (function #Inferencer.error as e -> e)
  in
  (* let* typedtree = Inferencer.structure ast |> promote_error in *)
  (* let typedtree = Typedtree.compact_expr typedtree in *)
  let anf = Compile_lib.ANF.(anf_stru typedtree |> simplify_stru) in
  Format.printf "After ANF transformation.\n%!";
  Format.printf "%a\n%!" Compile_lib.ANF.pp_stru anf;

  let prepare_rkmi input_file ttree =
    let module_name =
      if String.ends_with ~suffix:".ml" input_file then
        String.sub input_file 0 (String.length input_file - 3)
      else failwith "Can't get module name"
    in
    Out_channel.with_open_bin (module_name ^ ".rkmi") (fun _ch ->
        let info =
          List.map
            (fun vb ->
              match vb.Typedtree.tvb_pat with
              | Tpat_tuple _ -> assert false
              | Tpat_var name ->
                  let arity = Typedtree.arity_of_expr vb.tvb_body in
                  {
                    Compile_lib.Rkmi.name = Ident.to_string name;
                    arity;
                    typ = vb.Typedtree.tvb_typ;
                  })
            ttree
        in
        Compile_lib.Rkmi.output (module_name ^ ".rkmi") info)
  in

  if cfg.compile_only then prepare_rkmi the_module_file typedtree;
  Amd64_impl.codegen ~compile_only:cfg.compile_only rkmis anf cfg.out_file
  |> promote_error

let print_errors = function
  | #Miniml.Parsing.error as e -> Format.printf "%a\n%!" Parsing.pp_error e
  | #Miniml.Inferencer.error as e ->
      Format.printf "%a\n%!" Inferencer.pp_error e

let cfg = cfg_empty ()

let () =
  Arg.parse
    [
      ("-o", Arg.String (fun s -> cfg.out_file <- s), " set output file");
      ("-", Arg.Unit (fun () -> cfg.stdin_file <- true), " read from stdin");
      ( "-c",
        Arg.Unit (fun () -> cfg.compile_only <- true),
        " Dont wrap main into _start automatically" );
      ( "-vamd64",
        Arg.Unit (fun () -> Amd64_impl.set_verbose true),
        " verbose output of Amd64 backend" );
    ]
    (add_input_file cfg) "help";
  match frontend cfg with Result.Ok () -> () | Error err -> print_errors err
