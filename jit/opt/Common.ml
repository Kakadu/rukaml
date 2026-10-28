type config = { profile_file : string (** Written by [profile], read by [specialize]. *) }

let ( let* ) = Option.bind

module ValTbl = Hashtbl.Make (struct
    type t = Llvm.llvalue

    let equal (a : t) (b : t) = a == b
    let hash (a : t) = Hashtbl.hash (Obj.magic a : int)
  end)

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

(** Module -> functions -> blocks -> instructions; definitions only by default. *)
let fold_module_instrs ?(fn_filter = is_definition) f acc m =
  Llvm.fold_left_functions
    (fun acc fn -> if fn_filter fn then fold_fn_instrs f acc fn else acc)
    acc
    m
;;

let instrs_of_block bb = Llvm.fold_left_instrs (fun acc i -> i :: acc) [] bb |> List.rev
let instrs_of_fn fn = fold_fn_instrs (fun acc i -> i :: acc) [] fn |> List.rev

(** Explicitly defined functions, not declarations. *)
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

(** Groups values by key, preserving first-seen key order. *)
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

(** Parses the payload into a fresh context and module. *)
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

(** Serializes the module, failing if verification fails. *)
let payload_of m =
  match Llvm_analysis.verify_module m with
  | None -> Llvm.string_of_llmodule m
  | Some msg -> failwith ("opt: patched IR is invalid: " ^ String.trim msg)
;;

(** Parses the payload, runs [f ctx m], and re-serializes only when [f]
    returns [true]. *)
let with_parsed_ir (ir : IR.t) (f : Llvm.llcontext -> Llvm.llmodule -> bool) : IR.t =
  let ctx, m = parse_payload ir in
  Fun.protect
    ~finally:(fun () ->
      Llvm.dispose_module m;
      Llvm.dispose_context ctx)
    (fun () ->
       let changed = f ctx m in
       if changed then { ir with IR.payload = payload_of m } else ir)
;;

(** The callee is the last operand of a call. *)
let is_named ins fn_name =
  is_instr ins
  && is_op Llvm.Opcode.Call ins
  && Llvm.value_name (Llvm.operand ins (Llvm.num_operands ins - 1)) = fn_name
;;

(** Follows a [rukaml_alloc_closure -> rukaml_applyN -> ...] chain from [start];
    returns its head, the flattened args and the chain. *)
let walk start =
  let rec go cur args chain =
    (* operands: closure, arg count, args..., callee *)
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

(** Like [walk], but the chain root may be any non-[rukaml_applyN] value. *)
let walk_any start =
  let rec go cur args intermediates =
    let n_args = Llvm.num_operands cur - 3 in
    if n_args < 1
    then None
    else (
      let here = Array.init n_args (fun i -> Llvm.operand cur (2 + i)) in
      let args = Array.append here args in
      let prev = Llvm.operand cur 0 in
      if is_named prev "rukaml_applyN"
      then go prev args (prev :: intermediates)
      else Some (prev, args, intermediates))
  in
  go start [||] []
;;

(** Whether [v] is used as the closure argument of [rukaml_applyN]. *)
let is_used_as_apply_closure v =
  Llvm.fold_left_uses
    (fun acc u ->
       let user = Llvm.user u in
       acc || (is_named user "rukaml_applyN" && Llvm.operand user 0 == v))
    false
    v
;;

type dyn_site =
  { ins : Llvm.llvalue (** final rukaml_applyN call *)
  ; root_cl : Llvm.llvalue (** root closure value *)
  ; args : Llvm.llvalue array (** combined arguments across chain *)
  ; intermediates : Llvm.llvalue list (** intermediate applyN calls, oldest first *)
  }

(** Dynamic [rukaml_applyN] sites of a function. *)
let dyn_sites_of_fn fn : dyn_site list =
  fold_fn_instrs
    (fun acc ins ->
       if (not (is_named ins "rukaml_applyN")) || is_used_as_apply_closure ins
       then acc
       else (
         match walk_any ins with
         | Some (root_cl, args, intermediates)
           when (not (is_named root_cl "rukaml_alloc_closure"))
                && Array.length args >= 1
                && Array.length args <= 4
                && List.for_all (fun v -> num_uses v = 1) intermediates ->
           { ins; root_cl; args; intermediates } :: acc
         | _ -> acc))
    []
    fn
  |> List.rev
;;

let clear_function_body ctx fn =
  let void_ty = Llvm.void_type ctx in
  List.iter
    (fun ins ->
       if Llvm.type_of ins != void_ty
       then Llvm.replace_all_uses_with ins (Llvm.undef (Llvm.type_of ins));
       Llvm.delete_instruction ins)
    (instrs_of_fn fn);
  Array.iter Llvm.delete_block (Llvm.basic_blocks fn)
;;

let make_declaration ctx fn =
  clear_function_body ctx fn;
  Llvm.set_linkage Llvm.Linkage.External fn
;;

let extract_callee_name v =
  let name = Llvm.value_name v in
  if name <> ""
  then name
  else if Llvm.num_operands v > 0
  then Llvm.value_name (Llvm.operand v 0)
  else ""
;;

(** The function allocated by `call ... rukaml_alloc_closure ...`, i.e. its
    first argument, when the arity matches. *)
let target_of m alloc =
  if Llvm.num_operands alloc <> 3
  then None
  else
    let* arity = Llvm.int64_of_const (Llvm.operand alloc 1) in
    let fn_name = extract_callee_name (Llvm.operand alloc 0) in
    if fn_name = ""
    then None
    else (
      match Llvm.lookup_function fn_name m with
      | Some fn when Array.length (Llvm.params fn) = Int64.to_int arity ->
        Some (fn, Int64.to_int arity)
      | _ -> None)
;;

type site =
  { ins : Llvm.llvalue (** the full application to rewrite *)
  ; target : Llvm.llvalue (** the allocated or called function *)
  ; args : Llvm.llvalue array (** collected arguments, application order *)
  ; chain : Llvm.llvalue list (** [alloc :: intermediates], oldest first *)
  }

let delete_site_chain (s : site) =
  List.iter
    (fun v -> if num_uses v = 0 then Llvm.delete_instruction v)
    (s.ins :: List.rev s.chain)
;;

let chain_site_of m ~require_const ~is_hot ins =
  let* alloc, args, chain = walk ins in
  let* target, arity = target_of m alloc in
  if
    arity = Array.length args
    && arity = Array.length (Llvm.params target)
    && is_hot (Llvm.value_name target)
    && ((not require_const) || Array.exists Llvm.is_constant args)
    && List.for_all (fun v -> num_uses v = 1) chain
  then Some { ins; target; args; chain = alloc :: chain }
  else None
;;

let direct_site_of m ~require_const ~is_hot ins =
  if not (is_instr ins && is_op Llvm.Opcode.Call ins)
  then None
  else (
    let n_ops = Llvm.num_operands ins in
    if n_ops < 1
    then None
    else (
      let callee = Llvm.operand ins (n_ops - 1) in
      let fn_name = Llvm.value_name callee in
      if
        fn_name = ""
        || fn_name = "rukaml_applyN"
        || fn_name = "rukaml_alloc_closure"
        || String.contains fn_name '$'
      then None
      else (
        match Llvm.lookup_function fn_name m with
        | Some target when is_definition target ->
          let args = Array.init (n_ops - 1) (fun i -> Llvm.operand ins i) in
          if
            Array.length args = Array.length (Llvm.params target)
            && is_hot fn_name
            && ((not require_const) || Array.exists Llvm.is_constant args)
          then Some { ins; target; args; chain = [] }
          else None
        | _ -> None)))
;;

(** Clones [target] into a new function [name] of type [fty]: parameters with
    [keep.(i) = false] are replaced by the constants [args.(i)]. *)
let clone_specialized ctx m ~name ~fty ~keep ~args target =
  let olds = Llvm.basic_blocks target in
  if Array.length olds = 0
  then None
  else (
    let f' = Llvm.define_function name fty m in
    let tbl : Llvm.llvalue ValTbl.t = ValTbl.create 256 in
    let bv = Llvm.value_of_block in
    (* Create map between target params and new params *)
    let new_params = Llvm.params f' in
    let j = ref 0 in
    Array.iteri
      (fun i p ->
         if keep.(i)
         then (
           ValTbl.replace tbl p new_params.(!j);
           incr j)
         else ValTbl.replace tbl p args.(i))
      (Llvm.params target);
    (* Create copies of target bbs and map them to old ones *)
    ValTbl.replace tbl (bv olds.(0)) (bv (Llvm.entry_block f'));
    Array.iteri
      (fun i old ->
         if i > 0
         then (
           let nb = Llvm.append_block ctx (Llvm.value_name (bv old)) f' in
           ValTbl.replace tbl (bv old) (bv nb)))
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
                ValTbl.replace tbl ins np;
                phis := (ins, np) :: !phis)
              else (
                let c = Llvm.instr_clone ins in
                Llvm.insert_into_builder c (Llvm.value_name ins) bld;
                ValTbl.replace tbl ins c;
                pairs := (ins, c) :: !pairs))
           (instrs_of_block old))
      olds;
    (* Rewrite operands in cloned instructions *)
    List.iter
      (fun (oi, ni) ->
         for k = 0 to Llvm.num_operands oi - 1 do
           Option.iter (Llvm.set_operand ni k) (ValTbl.find_opt tbl (Llvm.operand oi k))
         done)
      !pairs;
    (* Incoming phi edges *)
    let remap v = Option.value (ValTbl.find_opt tbl v) ~default:v in
    List.iter
      (fun (opi, npi) ->
         List.iter
           (fun (v, b) ->
              let b' =
                match ValTbl.find_opt tbl (bv b) with
                | Some x -> Llvm.block_of_value x
                | None -> b
              in
              Llvm.add_incoming (remap v, b') npi)
           (Llvm.incoming opi))
      !phis;
    Some f')
;;
