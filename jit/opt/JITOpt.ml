type config =
  { profile_file : string
    (** Where the [profile] pass dumps counters (tab-separated
            "count<TAB>name" lines); what [specialize] reads back. *)
  }

let ( let* ) = Option.bind
let is_op op ins = Llvm.instr_opcode ins = op
let is_definition fn = not (Llvm.is_declaration fn)

let is_instr v =
  match Llvm.classify_value v with
  | Llvm.ValueKind.Instruction _ -> true
  | _ -> false
;;

let num_uses v = Llvm.fold_left_uses (fun n _ -> n + 1) 0 v

let fold_fn_instrs f acc fn =
  Llvm.fold_left_blocks (fun acc bb -> Llvm.fold_left_instrs f acc bb) acc fn
;;

let fold_module_instrs ?(fn_filter = is_definition) f acc m =
  Llvm.fold_left_functions
    (fun acc fn -> if fn_filter fn then fold_fn_instrs f acc fn else acc)
    acc
    m
;;

let instrs_of_block bb = Llvm.fold_left_instrs (fun acc i -> i :: acc) [] bb |> List.rev
let instrs_of_fn fn = fold_fn_instrs (fun acc i -> i :: acc) [] fn |> List.rev

let defined_functions m =
  Llvm.fold_left_functions
    (fun acc fn -> if is_definition fn then fn :: acc else acc)
    []
    m
  |> List.rev
;;

(* модуль -> функции -> блоки -> инструкции; по умолчанию только определения *)
let fold_module_instrs ?(fn_filter = is_definition) f acc m =
  Llvm.fold_left_functions
    (fun acc fn -> if fn_filter fn then fold_fn_instrs f acc fn else acc)
    acc
    m
;;

let instrs_of_fn fn = fold_fn_instrs (fun acc i -> i :: acc) [] fn |> List.rev

let defined_functions m =
  Llvm.fold_left_functions
    (fun acc fn -> if is_definition fn then fn :: acc else acc)
    []
    m
  |> List.rev
;;

let first_instr bb =
  match Llvm.instr_begin bb with
  | Before instr -> Some instr
  | _ -> None
;;

let ret_type_of ctx fn =
  instrs_of_fn fn
  |> List.find_opt (is_op Llvm.Opcode.Ret)
  |> Option.map (fun r ->
    if Llvm.num_operands r = 0
    then Llvm.void_type ctx
    else Llvm.type_of (Llvm.operand r 0))
;;

let group_by key xs =
  let tbl = Hashtbl.create 8
  and order = ref [] in
  List.iter
    (fun x ->
       let k = key x in
       let prev =
         match Hashtbl.find_opt tbl k with
         | Some l -> l
         | None ->
           order := k :: !order;
           []
       in
       Hashtbl.replace tbl k (x :: prev))
    xs;
  List.rev_map (fun k -> k, List.rev (Hashtbl.find tbl k)) !order
;;

let parse_payload (ir : IR.t) =
  match IR.format ir with
  | IR.Bitcode -> failwith "opt: Bitcode payload is not supported yet"
  | IR.TextLL ->
    (* should I create ctx in here? *)
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
    let call_before hook arg instr =
      let b = Llvm.builder ctx in
      Llvm.position_before instr b;
      ignore (Llvm.build_call hook_ty hook [| arg |] "" b)
    in
    (* With opaque pointers the type of a global is [ptr], so the string
       globals are passed to the hooks directly — no GEP needed. *)
    List.iter
      (fun fn ->
         Option.iter
           (fun first ->
              let name = Llvm.value_name fn in
              let str =
                Llvm.define_global
                  ("rukaml_prof_fn." ^ name)
                  (Llvm.const_stringz ctx name)
                  m
              in
              call_before tick str first)
           (first_instr (Llvm.entry_block fn)))
      (defined_functions m);
    (match Llvm.lookup_function "main" m with
     | Some main_fn when is_definition main_fn ->
       (* сначала собираем все ret, потом меняем модуль *)
       (match List.filter (is_op Llvm.Opcode.Ret) (instrs_of_fn main_fn) with
        | [] -> ()
        | rets ->
          let path =
            Llvm.define_global
              "rukaml_prof_path"
              (Llvm.const_stringz ctx cfg.profile_file)
              m
          in
          List.iter (call_before dump path) rets)
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

  let is_named ins fn_name =
    is_instr ins
    && is_op Llvm.Opcode.Call ins
    && Llvm.value_name (Llvm.operand ins (Llvm.num_operands ins - 1)) = fn_name
  ;;

  (* Create a chain rukaml_alloc_closure -> rukaml_applyN -> ... -> rukaml_applyN;
    returns tuple containing head of the chain (`call ... rukaml_alloc_closure ...`), array of combined args and the chain as list *)
  let walk start =
    let rec go cur args chain =
      (* операнды: замыкание, число аргументов, аргументы..., callee *)
      let n_args = Llvm.num_operands cur - 3 in
      if n_args < 1
      then None
      else (
        let here = Array.init n_args (fun i -> Llvm.operand cur (2 + i)) in
        let args = Array.append here args in
        let prev = Llvm.operand cur 0 in
        if is_named prev "rukaml_applyN"
        then go prev args (prev :: chain)
        else if is_named prev "rukaml_alloc_closure"
        then Some (prev, args, chain)
        else None)
    in
    go start [||] []
  ;;

  (* [call ... callee] - last operand of call instruction is callee *)
  let is_named ins fn_name =
    is_instr ins
    && is_op Llvm.Opcode.Call ins
    && Llvm.value_name (Llvm.operand ins (Llvm.num_operands ins - 1)) = fn_name
  ;;

  (* Клонирует [target] в новую функцию [name] с типом [fty]: параметры, для
     которых [keep.(i) = false], заменяются константами [args.(i)]. *)
  let clone_specialized ctx m ~name ~fty ~keep ~args target =
    let olds = Llvm.basic_blocks target in
    if Array.length olds = 0
    then None
    else (
      let f' = Llvm.define_function name fty m in
      let tbl : (Llvm.llvalue, Llvm.llvalue) Hashtbl.t = Hashtbl.create 256 in
      let bv = Llvm.value_of_block in
      (* Create map between target params and new params *)
      let new_params = Llvm.params f' in
      let j = ref 0 in
      Array.iteri
        (fun i p ->
           if keep.(i)
           then (
             Hashtbl.replace tbl p new_params.(!j);
             incr j)
           else Hashtbl.replace tbl p args.(i))
        (Llvm.params target);
      (* Create copies of target bbs and map them to old ones *)
      Hashtbl.replace tbl (bv olds.(0)) (bv (Llvm.entry_block f'));
      Array.iteri
        (fun i old ->
           if i > 0
           then (
             let nb = Llvm.append_block ctx (Llvm.value_name (bv old)) f' in
             Hashtbl.replace tbl (bv old) (bv nb)))
        olds;
      (* Basic blocks of new function *)
      let news = Llvm.basic_blocks f' in
      (* List of instr pairs: (old, clone) *)
      let pairs = ref [] in
      (* List of phi pairs: (old, clone) *)
      let phis = ref [] in
      Array.iteri
        (fun i old ->
           let bld = Llvm.builder_at_end ctx news.(i) in
           List.iter
             (fun ins ->
                if is_op Llvm.Opcode.PHI ins
                then (
                  let np =
                    Llvm.build_empty_phi (Llvm.type_of ins) (Llvm.value_name ins) bld
                  in
                  Hashtbl.replace tbl ins np;
                  phis := (ins, np) :: !phis)
                else (
                  let c = Llvm.instr_clone ins in
                  Llvm.insert_into_builder c (Llvm.value_name ins) bld;
                  Hashtbl.replace tbl ins c;
                  pairs := (ins, c) :: !pairs))
             (instrs_of_block old))
        olds;
      (* перепривязываем операнды клонов *)
      List.iter
        (fun (oi, ni) ->
           for k = 0 to Llvm.num_operands oi - 1 do
             Option.iter
               (Llvm.set_operand ni k)
               (Hashtbl.find_opt tbl (Llvm.operand oi k))
           done)
        !pairs;
      (* входящие рёбра phi *)
      let remap v = Option.value (Hashtbl.find_opt tbl v) ~default:v in
      List.iter
        (fun (opi, npi) ->
           List.iter
             (fun (v, b) ->
                let b' =
                  match Hashtbl.find_opt tbl (bv b) with
                  | Some x -> Llvm.block_of_value x
                  | None -> b
                in
                Llvm.add_incoming (remap v, b') npi)
             (Llvm.incoming opi))
        !phis;
      Some f')
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
    (* Get the actual function from `call ... rukaml_alloc_closure ...` (first argument of rukaml_alloc_closure)*)
    let target_of alloc =
      if Llvm.num_operands alloc <> 3
      then None
      else
        let* arity = Llvm.int64_of_const (Llvm.operand alloc 1) in
        (* TODO: what if ptrtoint unwrapping isn't needed *)
        let fn_val = Llvm.operand (Llvm.operand alloc 0) 0 in
        match Llvm.lookup_function (Llvm.value_name fn_val) m with
        | Some fn when is_definition fn -> Some (fn, Int64.to_int arity)
        | _ -> None
    in
    (* Create site of instruction *)
    let site_of ins =
      let* alloc, args, chain = walk ins in
      let* target, arity = target_of alloc in
      if
        arity = Array.length args
        && arity = Array.length (Llvm.params target)
        && hot (Llvm.value_name target)
        && Array.exists Llvm.is_constant args
        && List.for_all (fun v -> num_uses v = 1) (alloc :: chain)
      then Some { ins; target; args; chain = alloc :: chain }
      else None
    in
    (* Gether all sites starting with `rukaml_applyN` *)
    let sites =
      fold_module_instrs
        (fun acc ins ->
           if not (is_named ins "rukaml_applyN")
           then acc
           else (
             match site_of ins with
             | Some s -> s :: acc
             | None -> acc))
        []
        m
      |> List.rev
    in
    (* Const key is a string containing info about const args of site *)
    let const_key args =
      Array.to_list args
      |> List.mapi (fun i a ->
        if Llvm.is_constant a
        then Printf.sprintf "%d=%s" i (Llvm.string_of_llvalue a)
        else "")
      |> String.concat ","
    in
    (* Grouping sites by same targets and const keys *)
    let groups = group_by (fun s -> Llvm.value_name s.target, const_key s.args) sites in
    (* Table of clone counters *)
    let clone_counter : (string, int) Hashtbl.t = Hashtbl.create 8 in
    let next_idx fname =
      let c = Option.value (Hashtbl.find_opt clone_counter fname) ~default:0 in
      Hashtbl.replace clone_counter fname (c + 1);
      c
    in
    (* Table of rewritten sites *)
    let repl : (Llvm.llvalue, Llvm.llvalue) Hashtbl.t = Hashtbl.create 64 in
    (* List of sites that have to be deleted *)
    let to_delete = ref [] in
    let rewrite_site ~fty ~f' ~keep s =
      let resolve a = Option.value (Hashtbl.find_opt repl a) ~default:a in
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
      Hashtbl.replace repl s.ins call;
      to_delete := (s.ins :: List.rev s.chain) :: !to_delete
    in
    let specialize_group ((fname, _), group) =
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
          let name = Printf.sprintf "%s$spec$%d" fname (next_idx fname) in
          let* f' = clone_specialized ctx m ~name ~fty ~keep ~args:s0.args s0.target in
          Some (fty, f')
        in
        Option.iter
          (fun (fty, f') -> List.iter (rewrite_site ~fty ~f' ~keep) group)
          specialized
    in
    List.iter specialize_group groups;
    match !to_delete with
    | [] -> ir
    | deletions ->
      List.iter (List.iter Llvm.delete_instruction) (List.rev deletions);
      { ir with IR.payload = payload_of m }
  ;;
end

type pipeline = (module Pass) list

let empty : pipeline = []

let pass ~name ~requires ~run : (module Pass) =
  (module struct
    let name = name
    let requires = requires
    let run = run
  end)
;;

let make (cfg : config) : string -> (module Pass) option = function
  | "profile" ->
    Some (pass ~name:Profile.name ~requires:Profile.requires ~run:(Profile.run cfg))
  | "specialize" ->
    Some
      (pass ~name:Specialize.name ~requires:Specialize.requires ~run:(Specialize.run cfg))
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

(** [run ?dump pipeline ir] folds the passes left to right. After every
    pass the optional [dump] callback is invoked with the pass name and its
    output — the raw [IR.t], before the [; opts=...] header is prepended —
    so callers can observe every intermediate step. *)
let run ?(dump = fun (_ : string) (_ : IR.t) -> ()) (p : pipeline) (ir : IR.t) : IR.t =
  match p with
  | [] -> ir
  | passes ->
    let ir' =
      List.fold_left
        (fun acc (module M : Pass) ->
           let acc = M.run acc in
           dump M.name acc;
           acc)
        ir
        passes
    in
    let opts = String.concat "," (names passes) in
    { ir' with
      IR.producer = ir'.IR.producer ^ "; opts=" ^ opts
    ; IR.payload = "; opts=" ^ opts ^ "\n" ^ IR.payload ir'
    }
;;
