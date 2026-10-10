type config =
  { profile_file : string
    (** Where the [profile] pass dumps counters (tab-separated
            "count<TAB>name" lines); what [specialize] reads back. *)
  }

let ret_type_of ctx fn =
  let scan bb =
    Llvm.fold_left_instrs
      (fun acc ins ->
         match acc with
         | Some _ -> acc
         | None ->
           if Llvm.instr_opcode ins = Llvm.Opcode.Ret
           then
             Some
               (if Llvm.num_operands ins = 0
                then Llvm.void_type ctx
                else Llvm.type_of (Llvm.operand ins 0))
           else None)
      None
      bb
  in
  let blocks = Llvm.basic_blocks fn in
  let rec loop i =
    if i >= Array.length blocks
    then None
    else (
      match scan blocks.(i) with
      | Some t -> Some t
      | None -> loop (i + 1))
  in
  loop 0
;;

let first_instr bb =
  Llvm.fold_left_instrs
    (fun acc ins ->
       match acc with
       | Some _ -> acc
       | None -> Some ins)
    None
    bb
;;

let parse_payload (ir : IR.t) =
  match IR.format ir with
  | IR.Bitcode -> failwith "opt: Bitcode payload is not supported yet"
  | IR.TextLL ->
    let ctx = Llvm.create_context () in
    let m =
      Llvm_irreader.parse_ir
        ctx
        (Llvm.MemoryBuffer.of_string ~name:"main" (IR.payload ir))
    in
    ctx, m
;;

let payload_of m =
  match Llvm_analysis.verify_module m with
  | None -> Llvm.string_of_llmodule m
  | Some msg -> failwith ("opt: patched IR is invalid: " ^ String.trim msg)
;;

module type Pass = sig
  (** CLI name of the pass, as in [--opt=<name>]. *)
  val name : string

  (** Runtime symbols the pass makes the module call; the driver checks
      them against [BackEngine.runtime_symbols] before [load]. *)
  val requires : string list

  val run : IR.t -> IR.t
end

module Profile = struct
  let name = "profile"
  let requires = [ "rukaml_prof_tick"; "rukaml_prof_dump" ]

  let run (cfg : config) (ir : IR.t) : IR.t =
    let ctx, m = parse_payload ir in
    let ptr = Llvm.pointer_type ctx in
    let hook_ty = Llvm.function_type (Llvm.void_type ctx) [| ptr |] in
    let tick = Llvm.declare_function "rukaml_prof_tick" hook_ty m in
    let dump = Llvm.declare_function "rukaml_prof_dump" hook_ty m in
    (* With opaque pointers the type of a global is [ptr], so the string
       globals are passed to the hooks directly — no GEP needed. *)
    let () =
      Llvm.fold_left_functions
        (fun () fn ->
           if Llvm.is_declaration fn
           then ()
           else (
             match first_instr (Llvm.entry_block fn) with
             | None -> ()
             | Some first ->
               let str =
                 Llvm.define_global
                   ("rukaml_prof_fn." ^ Llvm.value_name fn)
                   (Llvm.const_stringz ctx (Llvm.value_name fn))
                   m
               in
               let b = Llvm.builder ctx in
               Llvm.position_before first b;
               ignore (Llvm.build_call hook_ty tick [| str |] "" b)))
        ()
        m
    in
    (match Llvm.lookup_function "main" m with
     | Some main_fn when not (Llvm.is_declaration main_fn) ->
       let rets =
         Array.fold_left
           (fun acc bb ->
              Llvm.fold_left_instrs
                (fun acc ins ->
                   if Llvm.instr_opcode ins = Llvm.Opcode.Ret then ins :: acc else acc)
                acc
                bb)
           []
           (Llvm.basic_blocks main_fn)
         |> List.rev
       in
       if rets <> []
       then (
         let path =
           Llvm.define_global
             "rukaml_prof_path"
             (Llvm.const_stringz ctx cfg.profile_file)
             m
         in
         List.iter
           (fun ret ->
              let b = Llvm.builder ctx in
              Llvm.position_before ret b;
              ignore (Llvm.build_call hook_ty dump [| path |] "" b))
           rets)
     | _ -> ());
    { ir with IR.payload = payload_of m }
  ;;
end

module Specialize = struct
  let name = "specialize"
  let requires = []

  let hot_threshold = 4L

  let read_profile path : (string * int64) list =
    if not (Sys.file_exists path)
    then
      failwith
        (Printf.sprintf
           "specialize: profile file %s does not exist (run with --opt=profile first)"
           path);
    let ic = open_in path in
    let malformed line =
      close_in ic;
      failwith (Printf.sprintf "specialize: malformed profile line %S in %s" line path)
    in
    let rec loop acc =
      match input_line ic with
      | line ->
        if String.trim line = ""
        then loop acc
        else (
          match String.index_opt line '\t' with
          | Some i ->
            (match Int64.of_string_opt (String.sub line 0 i) with
             | Some n ->
               loop ((String.sub line (i + 1) (String.length line - i - 1), n) :: acc)
             | None -> malformed line)
          | None -> malformed line)
      | exception End_of_file ->
        close_in ic;
        acc
    in
    loop []
  ;;

  type site =
    { ins : Llvm.llvalue (** the full application to rewrite *)
    ; target : Llvm.llvalue (** the allocated function *)
    ; args : Llvm.llvalue array (** collected arguments, application order *)
    ; chain : Llvm.llvalue list (** [alloc :: intermediates], oldest first *)
    }

  let run (cfg : config) (ir : IR.t) : IR.t =
    let profile = read_profile cfg.profile_file in
    let hot fn_name =
      match List.assoc_opt fn_name profile with
      | Some n -> n >= hot_threshold
      | None -> false
    in
    let ctx, m = parse_payload ir in
    let defs : (Llvm.llvalue, Llvm.llvalue) Hashtbl.t = Hashtbl.create 4096 in
    let () =
      Llvm.fold_left_functions
        (fun () fn ->
           if not (Llvm.is_declaration fn)
           then
             Array.iter
               (fun bb ->
                  Llvm.fold_left_instrs (fun () ins -> Hashtbl.replace defs ins ins) () bb)
               (Llvm.basic_blocks fn))
        ()
        m
    in
    let is_call ins = Llvm.instr_opcode ins = Llvm.Opcode.Call in
    let is_named ins fn_name =
      is_call ins
      && Llvm.value_name (Llvm.operand ins (Llvm.num_operands ins - 1)) = fn_name
    in
    let walk start =
      let rec go cur args chain =
        let n_args = Llvm.num_operands cur - 3 in
        if n_args < 1
        then None
        else (
          let here = Array.init n_args (fun i -> Llvm.operand cur (2 + i)) in
          let args = Array.append here args in
          match Hashtbl.find_opt defs (Llvm.operand cur 0) with
          | Some prev when is_named prev "rukaml_applyN" -> go prev args (prev :: chain)
          | Some alloc when is_named alloc "rukaml_alloc_closure" ->
            Some (alloc, args, chain)
          | _ -> None)
      in
      go start [||] []
    in
    let target_of alloc =
      if Llvm.num_operands alloc <> 3
      then None
      else (
        match Llvm.int64_of_const (Llvm.operand alloc 1) with
        | None -> None
        | Some arity ->
          let fn_val = Llvm.operand (Llvm.operand alloc 0) 0 in
          (match Llvm.lookup_function (Llvm.value_name fn_val) m with
           | Some fn when not (Llvm.is_declaration fn) -> Some (fn, arity)
           | _ -> None))
    in
    let count_uses v =
      Llvm.fold_left_functions
        (fun acc fn ->
           if Llvm.is_declaration fn
           then acc
           else
             Array.fold_left
               (fun acc bb ->
                  Llvm.fold_left_instrs
                    (fun acc ins ->
                       let n = Llvm.num_operands ins in
                       let rec cnt k =
                         if k >= n
                         then 0
                         else (if Llvm.operand ins k = v then 1 else 0) + cnt (k + 1)
                       in
                       acc + cnt 0)
                    acc
                    bb)
               acc
               (Llvm.basic_blocks fn))
        0
        m
    in
    let sites =
      Llvm.fold_left_functions
        (fun acc fn ->
           if Llvm.is_declaration fn
           then acc
           else
             Array.fold_left
               (fun acc bb ->
                  Llvm.fold_left_instrs
                    (fun acc ins ->
                       if not (is_named ins "rukaml_applyN")
                       then acc
                       else (
                         match walk ins with
                         | None -> acc
                         | Some (alloc, args, chain) ->
                           (match target_of alloc with
                            | None -> acc
                            | Some (target, arity) ->
                              let arity = Int64.to_int arity in
                              if
                                arity <> Array.length args
                                || arity <> Array.length (Llvm.params target)
                                || not (hot (Llvm.value_name target))
                              then acc
                              else if not (Array.exists Llvm.is_constant args)
                              then acc
                              else if
                                List.exists (fun v -> count_uses v <> 1) (alloc :: chain)
                              then acc
                              else { ins; target; args; chain = alloc :: chain } :: acc)))
                    acc
                    bb)
               acc
               (Llvm.basic_blocks fn))
        []
        m
      |> List.rev
    in
    let const_key args =
      String.concat
        ","
        (Array.to_list
           (Array.mapi
              (fun i a ->
                 if Llvm.is_constant a
                 then string_of_int i ^ "=" ^ Llvm.string_of_llvalue a
                 else "")
              args))
    in
    let groups_tbl : (string * string, site list) Hashtbl.t = Hashtbl.create 8 in
    let groups_order = ref [] in
    List.iter
      (fun s ->
         let k = Llvm.value_name s.target, const_key s.args in
         (match Hashtbl.find_opt groups_tbl k with
          | Some ss -> Hashtbl.replace groups_tbl k (s :: ss)
          | None ->
            Hashtbl.replace groups_tbl k [ s ];
            groups_order := k :: !groups_order);
         ())
      sites;
    let groups =
      List.map (fun k -> k, List.rev (Hashtbl.find groups_tbl k)) (List.rev !groups_order)
    in
    let clone_counter : (string, int) Hashtbl.t = Hashtbl.create 8 in
    let rewritten = ref 0 in
    let repl : (Llvm.llvalue, Llvm.llvalue) Hashtbl.t = Hashtbl.create 64 in
    let to_delete = ref [] in
    List.iter
      (fun ((fname, _), group) ->
         match group with
         | [] -> ()
         | s0 :: _ ->
           let target = s0.target in
           let keep = Array.map (fun a -> not (Llvm.is_constant a)) s0.args in
           let ps = Llvm.params target in
           if Array.length s0.args <> Array.length ps
           then ()
           else (
             match ret_type_of ctx target with
             | None -> ()
             | Some ret_ty ->
               let kept_types = ref [] in
               Array.iteri
                 (fun i p -> if keep.(i) then kept_types := Llvm.type_of p :: !kept_types)
                 ps;
               let fty =
                 Llvm.function_type ret_ty (Array.of_list (List.rev !kept_types))
               in
               let idx =
                 match Hashtbl.find_opt clone_counter fname with
                 | Some c ->
                   Hashtbl.replace clone_counter fname (c + 1);
                   c
                 | None ->
                   Hashtbl.replace clone_counter fname 1;
                   0
               in
               let f' =
                 Llvm.define_function (Printf.sprintf "%s$spec$%d" fname idx) fty m
               in
               let tbl : (Llvm.llvalue, Llvm.llvalue) Hashtbl.t = Hashtbl.create 256 in
               let ps' = Llvm.params f' in
               let j = ref 0 in
               Array.iteri
                 (fun i p ->
                    if keep.(i)
                    then (
                      Hashtbl.replace tbl p ps'.(!j);
                      incr j)
                    else Hashtbl.replace tbl p s0.args.(i))
                 ps;
               let olds = Llvm.basic_blocks target in
               if Array.length olds = 0
               then ()
               else (
                 let bv = Llvm.value_of_block in
                 Hashtbl.replace tbl (bv olds.(0)) (bv (Llvm.entry_block f'));
                 for i = 1 to Array.length olds - 1 do
                   let nb = Llvm.append_block ctx (Llvm.value_name (bv olds.(i))) f' in
                   Hashtbl.replace tbl (bv olds.(i)) (bv nb)
                 done;
                 let news = Llvm.basic_blocks f' in
                 let pairs = ref [] in
                 let phis = ref [] in
                 for i = 0 to Array.length olds - 1 do
                   let insns =
                     Llvm.fold_left_instrs (fun acc ins -> ins :: acc) [] olds.(i)
                     |> List.rev
                   in
                   let bld = Llvm.builder_at_end ctx news.(i) in
                   List.iter
                     (fun ins ->
                        if Llvm.instr_opcode ins = Llvm.Opcode.PHI
                        then (
                          let np =
                            Llvm.build_empty_phi
                              (Llvm.type_of ins)
                              (Llvm.value_name ins)
                              bld
                          in
                          Hashtbl.replace tbl ins np;
                          phis := (ins, np) :: !phis)
                        else (
                          let c = Llvm.instr_clone ins in
                          Llvm.insert_into_builder c (Llvm.value_name ins) bld;
                          Hashtbl.replace tbl ins c;
                          pairs := (ins, c) :: !pairs))
                     insns
                 done;
                 List.iter
                   (fun (oi, ni) ->
                      for k = 0 to Llvm.num_operands oi - 1 do
                        match Hashtbl.find_opt tbl (Llvm.operand oi k) with
                        | Some op' -> Llvm.set_operand ni k op'
                        | None -> ()
                      done)
                   !pairs;
                 List.iter
                   (fun (opi, npi) ->
                      List.iter
                        (fun (v, b) ->
                           let v' =
                             match Hashtbl.find_opt tbl v with
                             | Some x -> x
                             | None -> v
                           in
                           let b' =
                             match Hashtbl.find_opt tbl (bv b) with
                             | Some x -> Llvm.block_of_value x
                             | None -> b
                           in
                           Llvm.add_incoming (v', b') npi)
                        (Llvm.incoming opi))
                   !phis;
                 List.iter
                   (fun s ->
                      let resolve a =
                        match Hashtbl.find_opt repl a with
                        | Some r -> r
                        | None -> a
                      in
                      let kept_args =
                        Array.of_list
                          (List.filteri (fun i _ -> keep.(i)) (Array.to_list s.args))
                        |> Array.map resolve
                      in
                      let b = Llvm.builder ctx in
                      Llvm.position_before s.ins b;
                      let call = Llvm.build_call fty f' kept_args "" b in
                      Llvm.replace_all_uses_with s.ins call;
                      Hashtbl.replace repl s.ins call;
                      to_delete := !to_delete @ (s.ins :: List.rev s.chain);
                      incr rewritten)
                   group)))
      groups;
    List.iter Llvm.delete_instruction !to_delete;
    if !rewritten = 0 then ir else { ir with IR.payload = payload_of m }
  ;;
end

type pipeline = (module Pass) list

let empty : pipeline = []

let make (cfg : config) : string -> (module Pass) option =
  fun n ->
  match n with
  | "profile" ->
    Some
      (module struct
        let name = Profile.name
        let requires = Profile.requires
        let run = Profile.run cfg
      end : Pass)
  | "specialize" ->
    Some
      (module struct
        let name = Specialize.name
        let requires = Specialize.requires
        let run = Specialize.run cfg
      end : Pass)
  | _ -> None
;;

let of_flags (cfg : config) (names : string list) : pipeline =
  List.map
    (fun n ->
       match make cfg n with
       | Some p -> p
       | None -> failwith (Printf.sprintf "unknown JIT optimization pass: %S" n))
    names
;;

let names (p : pipeline) = List.map (fun (module M : Pass) -> M.name) p

let requires (p : pipeline) = List.concat_map (fun (module M : Pass) -> M.requires) p

let missing_runtime ~available required =
  List.filter (fun sym -> not (List.mem sym available)) required
;;

let run (p : pipeline) (ir : IR.t) : IR.t =
  match p with
  | [] -> ir
  | passes ->
    let ir' = List.fold_left (fun acc (module M : Pass) -> M.run acc) ir passes in
    let opts = String.concat "," (names passes) in
    { ir' with
      IR.producer = ir'.IR.producer ^ "; opts=" ^ opts
    ; IR.payload = "; opts=" ^ opts ^ "\n" ^ IR.payload ir'
    }
;;
