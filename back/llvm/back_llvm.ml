let extra_switches = []

open Frontend

type error = [ `Typing_error of Inferencer.error ]

let pp_error ppf _ = Format.fprintf ppf "error?"

let prepare_llvm _anf ~filename:_ =
  print_endline "LLVM backend is running111";
  let context = Llvm.global_context () in
  let builder = Llvm.builder context in
  let the_module = Llvm.create_module context "main" in
  let module LL = (val LL.make context builder the_module) in
  Result.Ok ()
;;

let compile stru _stru_typed ~filename =
  let ( let* ) x f = Result.bind x f in
  (* Format.printf "Parsed: %a\n%!" pp stru; *)
  let stru =
    let init = CConv.standart_globals, [] in
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
        new_glob, List.append ans new_strus)
    |> snd
  in
  let* stru_typed =
    Inferencer.structure stru
    |> Result.map_error (function #Inferencer.error as e -> `Typing_error e)
  in
  (* Format.printf "After CCovv.\n%!"; *)
  (* Format.printf "%a\n%!" Pprinttyped.pp_typed stru_typed; *)
  let anf = Compile_lib.ANF.(anf_stru stru_typed |> simplify_stru) in
  Format.printf "After ANF transformation.\n%!";
  Format.printf "%a\n%!" Compile_lib.ANF.pp_stru anf;
  (* Result.return () *)
  prepare_llvm anf ~filename
;;

let () =
  let module B = struct
    type nonrec error = error

    let pp_error = pp_error
    let run = compile
  end
  in
  Registration.register_backend_exn "llvm" (module B) []
;;
