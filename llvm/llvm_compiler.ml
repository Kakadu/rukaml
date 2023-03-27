type cfg =
  { mutable object_out : string
  ; mutable llvm_out : string
  ; mutable inp : [ `Stdin | `File of string ]
  ; mutable emit_llvm : bool
  }

open Base
module Format = Stdlib.Format
open Miniml
open Llvm

module ToLLVM = struct
  let get_value_bindings text =
    let ( let* ) x f = Result.bind x ~f in
    let* stru = Miniml.Parsing.parse_structure text in
    let stru = List.concat_map ~f:CConv.conv stru in
    let f acc ((_, Parsetree.PVar name, _) as vb) =
      let* ans, env = acc in
      let* rez = Inferencer.vb ~env vb in
      Result.return (rez :: ans, (name, rez.tvb_typ) :: env)
    in
    List.fold stru ~init:(Result.return ([], Inferencer.start_env)) ~f
    |> Result.map ~f:(fun (vbs, _) -> List.rev vbs)
  ;;

  let ( let* ) x f = Result.bind x ~f

  let run cfg =
    let text =
      match cfg.inp with
      | `Stdin -> Stdio.In_channel.(input_all stdin) |> String.rstrip
      | `File fname -> Stdio.In_channel.with_file fname ~f:Stdio.In_channel.input_all
    in
    let promote_error r =
      Result.map_error
        ~f:(fun x -> (x :> [ Parsing.error | Inferencer.error | LLVMIR.error ]))
        r
    in
    let* value_bindings = get_value_bindings text in
    LLVMIR.generate_llvm value_bindings cfg.emit_llvm cfg.llvm_out cfg.object_out
    |> promote_error
  ;;
end

let cfg = { llvm_out = "out.ll"; object_out = "out.o"; inp = `Stdin; emit_llvm = false }

let print_errors = function
  | #Miniml.Parsing.error as e -> Format.printf "Parsing error: %a\n%!" Parsing.pp_error e
  | #Miniml.Inferencer.error as e ->
    Format.printf "Inferencer error: %a\n%!" Inferencer.pp_error e
  | #LLVMIR.error as e -> Format.printf "Compiler error: %a\n!" LLVMIR.pp_error e
;;

let () =
  Stdlib.Arg.parse
    [ ( "-emit-llvm"
      , Stdlib.Arg.String
          (fun x ->
            cfg.llvm_out <- x;
            cfg.emit_llvm <- true)
      , "" )
    ; "-o", Stdlib.Arg.String (fun x -> cfg.object_out <- x), ""
    ]
    (fun s -> cfg.inp <- `File s)
    "help";
  match ToLLVM.run cfg with
  | Result.Ok () -> ()
  | Error err -> print_errors err
;;
