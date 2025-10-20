open! Base

open Frontend
open Compile_lib

let error fmt =
  let open Stdlib.Format in
  kfprintf
    (fun ppf ->
       pp_print_newline ppf ();
       Stdlib.exit 1)
    err_formatter
    fmt
;;

(**
  This module forms some kind of a DSL that can be used
  to describe different compilation targets.

  E.g (parse text) (cps ~caa) cconv infer anf rv64 (to_file "a.as")
*)
module Compiler = struct
  (** Encapsulates generated code. Requires output file path *)
  type code = path:string -> unit

  type _ t =
    | Parsetree : Parsetree.structure -> Parsetree.structure t
    | Typedtree : Typedtree.structure -> Typedtree.structure t
    | ANF : ANF.vb list -> ANF.vb list t
    | Code : code -> code t

  let k x = fun k -> k x

  (** Parse text to parsetree *)
  let parse (text : string) =
    match Parsing.parse_structure text with
    | Ok x -> k (Parsetree x)
    | Error (`Parse_error msg) -> error "parse error: %s" msg
  ;;

  (** Perform cps conversion on parsetree *)
  let cps (Parsetree stru) ~(caa : bool) =
    let stru =
      match CPSConv.cps_conv_program stru with
      | Ok x -> x
      | Error err -> error "cps error: %a" CPSConv.pp_error err
    in
    let vb =
      if caa
      then
        let open CPSLang.MACPS in
        cps_vb_to_parsetree_vb (CAA.call_arity_anal stru)
      else
        let open CPSLang.OneACPS in
        cps_vb_to_parsetree_vb stru
    in
    k (Parsetree [ vb ])
  ;;

  (** Perform closure conversion *)
  let cconv =
    let collect_globals =
      List.fold_left ~f:(fun acc -> function
        | _, Parsetree.PVar s, _ -> CConv.String_set.add s acc
        | _, PTuple _, _ -> acc)
    in
    let f (globals, acc) vb =
      let stru = CConv.conv ~standart_globals:globals vb in
      let globals = collect_globals ~init:globals stru in
      globals, List.append acc stru
    in
    fun (Parsetree stru) ->
      let _, stru = List.fold_left stru ~init:(CConv.standart_globals, []) ~f in
      k (Parsetree stru)
  ;;

  (** Infer parsetree to typedtree *)
  let infer (Parsetree stru) =
    match Inferencer.structure stru with
    | Ok x -> k (Typedtree x)
    | Error err -> error "infer error: %a" Inferencer.pp_error err
  ;;

  (** Perform ANF transformation on typedtree *)
  let anf (Typedtree stru) =
    let anf = ANF.simplify_stru (ANF.anf_stru stru) in
    k (ANF anf)
  ;;

  (** Generate code for RV64 *)
  let rv64 (ANF stru) =
    let f ~path =
      RV64_impl.codegen ~wrap_main_into_start:false stru path |> Result.ok_or_failwith
    in
    k (Code f)
  ;;

  (** Generate code for AMD64 *)
  let amd64 (ANF stru) =
    let f ~path =
      Amd64_impl.codegen ~wrap_main_into_start:true stru path |> Result.ok_or_failwith
    in
    k (Code f)
  ;;

  (** Generate code for LLVM *)
  let llvm (ANF stru) =
    let f ~path = LLVM_impl.codegen stru path |> Result.ok_or_failwith in
    k (Code f)
  ;;

  (** Put the text result of the functions above to file *)
  let to_file : type a. string -> a t -> unit =
    fun path ->
    let with_ppf f =
      Out_channel.with_open_text path (fun ch ->
        f (Stdlib.Format.formatter_of_out_channel ch))
    in
    function
    | Parsetree stru -> with_ppf (fun ppf -> Pprint.pp_stru ppf stru)
    | Typedtree stru -> with_ppf (fun ppf -> Pprinttyped.pp_stru ppf stru)
    | ANF stru -> with_ppf (fun ppf -> ANF.pp_stru ppf stru)
    | Code f -> f ~path
  ;;
end

(** All supported compilation targets *)
module Target = struct
  type params =
    { text : string
    ; out_path : string
    ; cps : bool
    ; caa : bool
    }

  open Compiler

  (** Intermediate targets *)
  module Intermediate = struct
    let parsetree (p : params) = parse p.text
    let cpstree p = (parsetree p) (if p.cps then cps ~caa:p.caa else ( |> ))
    let cconvtree p = (cpstree p) cconv
    let typedtree p = (cconvtree p) infer
    let anftree p = (typedtree p) anf
  end

  let rv64 p = (Intermediate.anftree p) rv64
  let amd64 p = (Intermediate.anftree p) amd64
  let llvm p = (Intermediate.anftree p) llvm

  let finish target p = (target p) (to_file p.out_path)

  let targets =
    Map.of_alist_exn
      (module String)
      [ "rv64", finish rv64
      ; "amd64", finish amd64
      ; "llvm", finish llvm
      ; "parsetree", finish Intermediate.parsetree
      ; "cps", finish Intermediate.cpstree
      ; "cconv", finish Intermediate.cconvtree
      ; "typedtree", finish Intermediate.typedtree
      ; "anf", finish Intermediate.anftree
      ]
  ;;
end

let print_targets () =
  let open Stdlib.Format in
  printf
    "supported targets:@ %a@."
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_string)
    (Map.keys Target.targets);
  Stdlib.exit 0
;;

(* TODO: fix backends so this is not needed *)
let hack = function
  | "amd64" ->
    Compile_lib.ANF.disable_arity_inline ();
    Compile_lib.ANF.disable_cmp_into_if_inline ()
  | "llvm" -> Compile_lib.ANF.disable_arity_inline ()
  | _ -> ()
;;

let () =
  let inp_path = ref None in
  let out_path = ref "a.out" in
  let target = ref "" in
  let cps = ref false in
  let caa = ref false in

  let open Stdlib.Arg in
  let args =
    [ "-o", Set_string out_path, " output file"
    ; "--target", Set_string target, " compilation target"
    ; "--print-targets", Unit print_targets, " print all supported targets"
    ; "--cps", Set cps, " enable cps conversion"
    ; "--caa", Set caa, " enable call arity analysis"
    ]
  in
  parse args (fun s -> inp_path := Some s) "rukaml";

  hack !target;

  let text =
    match !inp_path with
    | Some path -> In_channel.with_open_text path In_channel.input_all
    | None -> In_channel.input_all In_channel.stdin
  in

  let params = Target.{ text; out_path = !out_path; cps = !cps; caa = !caa } in
  match Map.find Target.targets !target with
  | Some target -> target params
  | None -> error "invalid target %S" !target
;;
