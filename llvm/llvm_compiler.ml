type cfg = {
  mutable out_file : string;
  mutable input_file : string option;
  mutable dump_ir : bool;
}

open Miniml

module ToLLVM = struct
  let run cfg =
    let text =
      match cfg.input_file with
      | Some filename ->
          Stdio.In_channel.with_file filename ~f:Stdio.In_channel.input_all
      | None -> Stdio.In_channel.(input_all stdin)
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
      Inferencer.structure stru
      |> Result.map_error (function #Inferencer.error as e -> e)
    in
    (* let* typedtree = Inferencer.structure ast |> promote_error in *)
    (* let typedtree = Typedtree.compact_expr typedtree in *)
    let anf = Compile_lib.ANF2.(anf_stru typedtree |> simplify_stru) in
    Format.printf "After ANF transformation.\n%!";
    Format.printf "%a\n%!" Compile_lib.ANF2.pp_stru anf;
    LLVM_impl.codegen anf cfg.out_file |> promote_error
end

let cfg = { out_file = "aaa.ll"; input_file = None; dump_ir = false }

let print_errors = function
  | #Miniml.Parsing.error as e -> Format.printf "%a\n%!" Parsing.pp_error e
  | #Miniml.Inferencer.error as e ->
      Format.printf "%a\n%!" Inferencer.pp_error e

let () =
  Arg.parse
    [
      ("-o", Arg.String (fun s -> cfg.out_file <- s), " set output file");
      ("-dump-ir", Arg.String (fun s -> cfg.out_file <- s), " ");
      ( "-vllvm",
        Arg.Unit (fun () -> LLVM_impl.set_verbose true),
        " verbose output of LLVM backend" );
    ]
    (fun s -> cfg.input_file <- Some s)
    "help";
  match ToLLVM.run cfg with Result.Ok () -> () | Error err -> print_errors err
