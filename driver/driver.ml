open! Base
open Stdio
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
    | ANF : ANF.stru -> ANF.stru t
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
    (* extracts value_bindings from other structure_items to perform cps conv on it *)
    let vbs =
      let aux = function
        | Parsetree.Pstr_value vb -> Some vb
        | _ -> None
      in
      Stdlib.List.filter_map aux stru
    in
    let vbs =
      match CPSConv.cps_conv vbs with
      | Error err -> error "cps error: %a" CPSConv.pp_error err
      | Ok vbs when caa ->
        let open CPSLang.MACPS in
        List.map ~f:cps_vb_to_parsetree_vb (CAA.call_arity_anal vbs)
      | Ok vbs ->
        let open CPSLang.OneACPS in
        List.map ~f:cps_vb_to_parsetree_vb vbs
    in
    let open Parsetree in
    (* merges structure items back together *)
    let rec merge acc = function
      | [], [] -> List.rev acc
      | Pstr_type td :: rest, macps_vbs -> merge (Pstr_type td :: acc) (rest, macps_vbs)
      | Pstr_value _ :: rest, macps_vb :: macps_vbs ->
        merge (Pstr_value macps_vb :: acc) (rest, macps_vbs)
      | [], _ :: _ | Pstr_value _ :: _, [] -> assert false
    in
    let stru = merge [] (stru, vbs) in
    k (Parsetree stru)
  ;;

  (** Perform closure conversion *)
  let cconv =
    let collect_globals =
      let rec collect_from_patt acc = function
        | Parsetree.PAny | PConst _ | PUnit -> acc
        | PVar name -> CConv.String_set.add name acc
        | PTuple (p1, p2, ps) -> List.fold ~f:collect_from_patt ~init:acc (p1 :: p2 :: ps)
        | PConstruct (_, args) -> List.fold ~f:collect_from_patt ~init:acc args
      in
      List.fold ~f:(fun acc (_rec, lhs, _rhs) -> collect_from_patt acc lhs)
    in
    let f (globals, acc) = function
      | Parsetree.Pstr_value vb ->
        let stru = CConv.conv ~standart_globals:globals vb in
        let globals = collect_globals ~init:globals stru in
        globals, List.append acc (List.map ~f:(fun vb -> Parsetree.Pstr_value vb) stru)
      | Parsetree.Pstr_type _ as td -> globals, List.append acc [ td ]
    in
    fun (Parsetree stru) ->
      let _, stru = List.fold_left stru ~init:(CConv.standart_globals, []) ~f in
      k (Parsetree stru)
  ;;

  (** Infer parsetree to typedtree *)
  let infer table (Parsetree stru) =
    match Inferencer.structure table stru with
    | Ok (_env, x) -> k (Typedtree x)
    | Error err -> error "infer error: %a" Inferencer.pp_error err
  ;;

  (** Perform ANF transformation on typedtree *)
  let anf (Typedtree stru) =
    let anf = ANF.simplify_stru (ANF.anf_stru stru) in
    k (ANF anf)
  ;;

  (** Generate code for RV64 *)
  let rv64 (ANF stru) =
    let vbs =
      List.map
        ~f:(function
          | ANF.ANF_vb (flg, Apat_var name, body) -> flg, name, body
          | _ -> failwith "not implemented")
        stru
    in
    let f ~path =
      RV64_impl.codegen ~wrap_main_into_start:false vbs path |> Result.ok_or_failwith
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
    let vbs =
      List.map
        ~f:(function
          | ANF.ANF_vb vb -> vb)
        stru
    in
    let f ~path = LLVM_impl.codegen vbs path |> Result.ok_or_failwith in
    k (Code f)
  ;;

  (** Put the text result of the functions above to file *)
  let to_file : type a. string -> a t -> unit =
    fun path ->
    let with_ppf f =
      (* for some reason pprint struggles with writing
         directly to file so i'm doing it the ugly way *)
      let open Stdlib.Format in
      f str_formatter;
      Out_channel.write_all path ~data:(flush_str_formatter ())
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
    ; ppx : bool
    }

  open Compiler

  (** Intermediate targets *)
  module Intermediate = struct
    let parsetree (p : params) =
      parse (if p.ppx then Parsing.make_preprocessing_exn p.text else p.text)
    ;;

    let cpstree p = (parsetree p) (if p.cps then cps ~caa:p.caa else ( |> ))
    let cconvtree p = (cpstree p) cconv
    let typedtree table p = (cconvtree p) (infer table)
    let anftree table p = (typedtree table p) anf
  end

  let rv64 table p = (Intermediate.anftree table p) rv64
  let amd64 table p = (Intermediate.anftree table p) amd64
  let llvm table p = (Intermediate.anftree table p) llvm
  let finish target p = (target p) (to_file p.out_path)

  let targets table =
    Map.of_alist_exn
      (module String)
      [ "rv64", finish (rv64 table)
      ; "amd64", finish (amd64 table)
      ; "llvm", finish (llvm table)
      ; "parsetree", finish Intermediate.parsetree
      ; ("cps", fun p -> finish Intermediate.cpstree { p with cps = true })
      ; "cconv", finish Intermediate.cconvtree
      ; "typedtree", finish Intermediate.(typedtree table)
      ; "anf", finish Intermediate.(anftree table)
      ]
  ;;
end

let print_targets () =
  let open Stdlib.Format in
  printf
    "supported targets:@ %a@."
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_string)
    (Map.keys (Target.targets Typedtree.empty_table));
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
  let ppx = ref true in
  let open Stdlib.Arg in
  let args =
    [ "-o", Set_string out_path, " output file"
    ; "--target", Set_string target, " compilation target"
    ; "--print-targets", Unit print_targets, " print all supported targets"
    ; "--cps", Set cps, " enable cps conversion"
    ; "--caa", Set caa, " enable call arity analysis"
    ; "--no-ppx", Set ppx, " disable preprocessing"
    ]
  in
  parse args (fun s -> inp_path := Some s) "rukaml";
  hack !target;
  let text =
    match !inp_path with
    | Some path -> In_channel.with_file path ~f:In_channel.input_all
    | None -> In_channel.input_all stdin
  in
  let params =
    Target.{ text; out_path = !out_path; cps = !cps; caa = !caa; ppx = !ppx }
  in
  match Map.find (Target.targets Typedtree.empty_table) !target with
  | Some target -> target params
  | None -> error "invalid target %S" !target
;;
