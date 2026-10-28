open Common

let name = "specialize"
let requires = []
let hot_threshold = 4L

type state =
  { clone_counter : (string, int) Hashtbl.t
  ; memo : (string * string, string) Hashtbl.t
  }

let create_state () : state =
  { clone_counter = Hashtbl.create 8; memo = Hashtbl.create 16 }
;;

(** Parses the tab-separated ["count\tname"] lines written by [Profile]. *)
let read_profile path : (string * int64) list =
  if not (Sys.file_exists path)
  then
    failwith
      (Printf.sprintf
         "specialize: profile file %s does not exist (run with --opt=profile first)"
         path);
  let malformed line =
    failwith (Printf.sprintf "specialize: malformed profile line %S in %s" line path)
  in
  let parse line =
    let* i = String.index_opt line '\t' in
    let* n = Int64.of_string_opt (String.sub line 0 i) in
    Some (String.sub line (i + 1) (String.length line - i - 1), n)
  in
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
       let rec loop acc =
         match input_line ic with
         | exception End_of_file -> acc
         | line when String.trim line = "" -> loop acc
         | line ->
           (match parse line with
            | Some entry -> loop (entry :: acc)
            | None -> malformed line)
       in
       loop [])
;;

let const_key args =
  Array.to_list args
  |> List.mapi (fun i a ->
    if Llvm.is_constant a
    then Printf.sprintf "%d=%s" i (Llvm.string_of_llvalue a)
    else "")
  |> String.concat ","
;;

(** Specializes calls to hot functions with constant arguments into cloned
    [name$spec$N] functions; returns whether the module changed. *)
let run_on_module ?(state = create_state ()) ~is_hot ctx m : bool =
  let site_of ins =
    let s_opt =
      if is_named ins "rukaml_applyN"
      then chain_site_of m ~require_const:true ~is_hot ins
      else direct_site_of m ~require_const:true ~is_hot ins
    in
    match s_opt with
    | Some s when is_definition s.target -> Some s
    | _ -> None
  in
  let sites =
    fold_module_instrs
      (fun acc ins ->
         match site_of ins with
         | Some s -> s :: acc
         | None -> acc)
      []
      m
    |> List.rev
  in
  match sites with
  | [] -> false
  | _ ->
    let groups = group_by (fun s -> Llvm.value_name s.target, const_key s.args) sites in
    let next_idx fname =
      let c = Option.value (Hashtbl.find_opt state.clone_counter fname) ~default:0 in
      Hashtbl.replace state.clone_counter fname (c + 1);
      c
    in
    let repl : Llvm.llvalue ValTbl.t = ValTbl.create 64 in
    let changed = ref false in
    let rewrite_site ~fty ~f' ~keep s =
      let resolve a = Option.value (ValTbl.find_opt repl a) ~default:a in
      let kept_args =
        Array.to_list s.args
        |> List.filteri (fun i _ -> keep.(i))
        |> List.map resolve
        |> Array.of_list
      in
      let b = Llvm.builder ctx in
      Llvm.position_before s.ins b;
      let call = Llvm.build_call fty f' kept_args "" b in
      Llvm.replace_all_uses_with s.ins call;
      ValTbl.replace repl s.ins call;
      delete_site_chain s;
      changed := true
    in
    let specialize_group ((fname, ckey), group) =
      match group with
      | [] -> ()
      | s0 :: _ ->
        let keep = Array.map (fun a -> not (Llvm.is_constant a)) s0.args in
        let specialized =
          let* ret_ty = ret_type_of ctx s0.target in
          let kept_types =
            Array.to_list (Llvm.params s0.target)
            |> List.filteri (fun i _ -> keep.(i))
            |> List.map Llvm.type_of
          in
          let fty = Llvm.function_type ret_ty (Array.of_list kept_types) in
          let name =
            match Hashtbl.find_opt state.memo (fname, ckey) with
            | Some existing -> existing
            | None ->
              let n = Printf.sprintf "%s$spec$%d" fname (next_idx fname) in
              Hashtbl.replace state.memo (fname, ckey) n;
              n
          in
          match Llvm.lookup_function name m with
          | Some existing_fn -> Some (fty, existing_fn)
          | None ->
            let* f' =
              clone_specialized ctx m ~name ~fty ~keep ~args:s0.args s0.target
            in
            Some (fty, f')
        in
        Option.iter
          (fun (fty, f') -> List.iter (rewrite_site ~fty ~f' ~keep) group)
          specialized
    in
    List.iter specialize_group groups;
    !changed
;;

let run (cfg : config) (ir : IR.t) : IR.t =
  let profile = read_profile cfg.profile_file in
  let is_hot fn_name =
    match List.assoc_opt fn_name profile with
    | Some n -> n >= hot_threshold
    | None -> false
  in
  with_parsed_ir ir (fun ctx m -> run_on_module ~is_hot ctx m)