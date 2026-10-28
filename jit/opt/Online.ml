open Common

(** Rewrite direct calls inside [fn] to [orig_name] into direct calls to
    [new_fn] when all specialized (!keep) arguments equal [const_args]. *)
let redirect_self_calls ctx ~orig_name ~new_fn ~new_fty ~keep ~const_args fn =
  let calls =
    fold_fn_instrs
      (fun acc ins ->
         if is_named ins orig_name
         then (
           let n_ops = Llvm.num_operands ins in
           let args = Array.init (n_ops - 1) (fun i -> Llvm.operand ins i) in
           if
             (* Check if args are the same *)
             Array.length args = Array.length keep
             && Array.for_all
                  (fun b -> b)
                  (Array.mapi (fun i k -> k || args.(i) == const_args.(i)) keep)
           then (ins, args) :: acc
           (* Skip self call with different args *)
           else acc)
         else acc)
      []
      fn
    |> List.rev
  in
  List.iter
    (fun (ins, args) ->
       let kept_args =
         Array.to_list args |> List.filteri (fun i _ -> keep.(i)) |> Array.of_list
       in
       let b = Llvm.builder ctx in
       Llvm.position_before ins b;
       let call = Llvm.build_call new_fty new_fn kept_args "" b in
       Llvm.replace_all_uses_with ins call;
       Llvm.delete_instruction ins)
    calls
;;

(** Build an internal inline-cache helper for a monomorphic dynamic applyN
    site and return the helper function + its lltype. *)
let build_ic_helper ctx m ~ic_name ~callee_fn ~argsc ~miss_arr_ty ~miss_glob ~func_id =
  let i64_ty = Llvm.i64_type ctx in
  let i8_ty = Llvm.i8_type ctx in
  let ptr_ty = Llvm.pointer_type ctx in
  let zero = Llvm.const_int i64_ty 0 in
  let one = Llvm.const_int i64_ty 1 in
  let param_tys = Array.make (1 + argsc) i64_ty in
  let ic_fty = Llvm.function_type i64_ty param_tys in
  let ic_fn = Llvm.define_function ic_name ic_fty m in
  Llvm.set_linkage Llvm.Linkage.Internal ic_fn;
  let entry_bb = Llvm.entry_block ic_fn in
  let fast_bb = Llvm.append_block ctx "ic_fast" ic_fn in
  let slow_bb = Llvm.append_block ctx "ic_slow" ic_fn in
  let cl_val = Llvm.param ic_fn 0 in
  let call_args = Array.init argsc (fun i -> Llvm.param ic_fn (1 + i)) in
  (* entry: check cl->code == callee_fn and shape_at_offset_8 == argsc *)
  let b_ent = Llvm.builder_at_end ctx entry_bb in
  let cl_ptr = Llvm.build_inttoptr cl_val ptr_ty "" b_ent in
  let code_ptr = Llvm.build_load ptr_ty cl_ptr "" b_ent in
  Llvm.set_alignment 8 code_ptr;
  let shape_ptr =
    Llvm.build_in_bounds_gep i8_ty cl_ptr [| Llvm.const_int i64_ty 8 |] "" b_ent
  in
  let shape_val = Llvm.build_load i64_ty shape_ptr "" b_ent in
  Llvm.set_alignment 4 shape_val;
  let ok_code = Llvm.build_icmp Llvm.Icmp.Eq code_ptr callee_fn "" b_ent in
  let ok_shape =
    Llvm.build_icmp Llvm.Icmp.Eq shape_val (Llvm.const_int i64_ty argsc) "" b_ent
  in
  let ok = Llvm.build_and ok_code ok_shape "" b_ent in
  ignore (Llvm.build_cond_br ok fast_bb slow_bb b_ent);
  (* fast: direct call to callee_fn *)
  let b_fast = Llvm.builder_at_end ctx fast_bb in
  let callee_fty = Llvm.function_type i64_ty (Array.make argsc i64_ty) in
  let r_fast = Llvm.build_call callee_fty callee_fn call_args "" b_fast in
  ignore (Llvm.build_ret r_fast b_fast);
  (* slow: increment @rukaml.miss[func_id] and run rukaml_applyN *)
  let b_slow = Llvm.builder_at_end ctx slow_bb in
  let miss_ptr =
    Llvm.build_in_bounds_gep
      miss_arr_ty
      miss_glob
      [| zero; Llvm.const_int i64_ty func_id |]
      ""
      b_slow
  in
  ignore
    (Llvm.build_atomicrmw
       Llvm.AtomicRMWBinOp.Add
       miss_ptr
       one
       Llvm.AtomicOrdering.Monotonic
       false
       ""
       b_slow);
  let apply_ty = Llvm.var_arg_function_type i64_ty [| i64_ty; i64_ty |] in
  let apply_fn = Llvm.declare_function "rukaml_applyN" apply_ty m in
  let apply_operands = Array.append [| cl_val; Llvm.const_int i64_ty argsc |] call_args in
  let r_slow = Llvm.build_call apply_ty apply_fn apply_operands "" b_slow in
  ignore (Llvm.build_ret r_slow b_slow);
  ic_fty, ic_fn
;;

(** Build an optimized delta module for [meta.funcs.(func_id)] at version [ver].
    [site_targets] maps [ord_in_fn -> (target_func_name, argsc)]. *)
let build_delta
      ~(master_ir : IR.t)
      ~(meta : Tier0.meta)
      ~(func_id : int)
      ~(ver : int)
      ~(prev_fallback : string)
      ~(arg_profile : int64 option array)
      ~(site_targets : (int * (string * int)) list)
  : IR.t * string * bool
  =
  let info = meta.funcs.(func_id) in
  let fname = info.name in
  let ver_name = Printf.sprintf "%s.v%d" fname ver in
  let ctx, m = parse_payload master_ir in
  Fun.protect
    ~finally:(fun () ->
      Llvm.dispose_module m;
      Llvm.dispose_context ctx)
    (fun () ->
       let i64_ty = Llvm.i64_type ctx in
       let zero = Llvm.const_int i64_ty 0 in
       let one = Llvm.const_int i64_ty 1 in
       let n_slots = max 1 (Array.length meta.funcs) in
       let miss_arr_ty = Llvm.array_type i64_ty n_slots in
       let miss_glob = Llvm.declare_global miss_arr_ty "rukaml.miss" m in
       let orig_fn =
         match Llvm.lookup_function fname m with
         | Some f when is_definition f -> f
         | _ -> failwith ("online: function not found in master IR: " ^ fname)
       in
       let orig_params = Llvm.params orig_fn in
       let arity = Array.length orig_params in
       let orig_fty = Llvm.function_type i64_ty (Array.make arity i64_ty) in
       let all_keep = Array.make arity true in
       let work_fn =
         match
           clone_specialized
             ctx
             m
             ~name:("__work_" ^ fname)
             ~fty:orig_fty
             ~keep:all_keep
             ~args:[||]
             orig_fn
         with
         | Some f -> f
         | None -> failwith ("online: failed to clone " ^ fname)
       in
       (* Phase 6: Inline guards on dynamic rukaml_applyN sites in work_fn *)
       let closure_params : (int, unit) Hashtbl.t = Hashtbl.create 4 in
       let repl_ic : Llvm.llvalue ValTbl.t = ValTbl.create 16 in
       let resolve_ic a = Option.value (ValTbl.find_opt repl_ic a) ~default:a in
       let dsites = dyn_sites_of_fn work_fn in
       let work_params = Llvm.params work_fn in
       let used_ic = ref false in
       List.iteri
         (fun ord (ds : dyn_site) ->
            Array.iteri
              (fun pi p -> if p == ds.root_cl then Hashtbl.replace closure_params pi ())
              work_params;
            match List.assoc_opt ord site_targets with
            | Some (callee_name, argsc) when Array.length ds.args = argsc ->
              (match Llvm.lookup_function callee_name m with
               | Some callee_fn ->
                 let ic_name = Printf.sprintf "__ic_%s_v%d_%d" fname ver ord in
                 let ic_fty, ic_fn =
                   build_ic_helper
                     ctx
                     m
                     ~ic_name
                     ~callee_fn
                     ~argsc
                     ~miss_arr_ty
                     ~miss_glob
                     ~func_id
                 in
                 let b = Llvm.builder ctx in
                 Llvm.position_before ds.ins b;
                 let root_cl = resolve_ic ds.root_cl in
                 let args = Array.map resolve_ic ds.args in
                 let ic_args = Array.append [| root_cl |] args in
                 let call = Llvm.build_call ic_fty ic_fn ic_args "" b in
                 Llvm.replace_all_uses_with ds.ins call;
                 ValTbl.replace repl_ic ds.ins call;
                 List.iter
                   (fun v -> if num_uses v = 0 then Llvm.delete_instruction v)
                   (ds.ins :: List.rev ds.intermediates);
                 used_ic := true
               | None -> ())
            | _ -> ())
         dsites;
       (* Phase 4: Devirtualize known alloc_closure -> applyN chains across module *)
       ignore (Devirt.run_on_module ctx m);
       (* Phase 5: Guarded entry specialization (excluding closure parameters already
          handled by Phase 6) *)
       let eff_prof =
         Array.init arity (fun i ->
           if Hashtbl.mem closure_params i
           then None
           else if i < Array.length arg_profile
           then arg_profile.(i)
           else None)
       in
       let has_const = Array.exists Option.is_some eff_prof in
       if not has_const
       then (
         Llvm.set_value_name ver_name work_fn;
         Llvm.set_linkage Llvm.Linkage.External work_fn;
         redirect_self_calls
           ctx
           ~orig_name:fname
           ~new_fn:work_fn
           ~new_fty:orig_fty
           ~keep:all_keep
           ~const_args:(Array.make arity zero)
           work_fn)
       else (
         let keep = Array.map Option.is_none eff_prof in
         let const_args =
           Array.map
             (function
               | Some v -> Llvm.const_of_int64 i64_ty v true
               | None -> zero)
             eff_prof
         in
         let kept_types =
           Array.to_list orig_params
           |> List.filteri (fun i _ -> keep.(i))
           |> List.map Llvm.type_of
           |> Array.of_list
         in
         let gspec_fty = Llvm.function_type i64_ty kept_types in
         let gspec_name = Printf.sprintf "%s$gspec$%d" fname ver in
         let gspec_fn =
           match
             clone_specialized
               ctx
               m
               ~name:gspec_name
               ~fty:gspec_fty
               ~keep
               ~args:const_args
               work_fn
           with
           | Some f -> f
           | None -> failwith ("online: failed to specialize " ^ fname)
         in
         Llvm.set_linkage Llvm.Linkage.Internal gspec_fn;
         clear_function_body ctx work_fn;
         Llvm.delete_function work_fn;
         redirect_self_calls
           ctx
           ~orig_name:fname
           ~new_fn:gspec_fn
           ~new_fty:gspec_fty
           ~keep
           ~const_args
           gspec_fn;
         (* Build dispatcher ver_name *)
         let disp_fn = Llvm.define_function ver_name orig_fty m in
         Llvm.set_linkage Llvm.Linkage.External disp_fn;
         let d_entry = Llvm.entry_block disp_fn in
         let d_fast = Llvm.append_block ctx "guard_fast" disp_fn in
         let d_slow = Llvm.append_block ctx "guard_slow" disp_fn in
         let b_ent = Llvm.builder_at_end ctx d_entry in
         let d_params = Llvm.params disp_fn in
         let cond_opt =
           Array.to_list eff_prof
           |> List.mapi (fun i opt -> i, opt)
           |> List.fold_left
                (fun acc (i, opt) ->
                   match opt with
                   | None -> acc
                   | Some _ ->
                     let cmp =
                       Llvm.build_icmp Llvm.Icmp.Eq d_params.(i) const_args.(i) "" b_ent
                     in
                     (match acc with
                      | None -> Some cmp
                      | Some prev -> Some (Llvm.build_and prev cmp "" b_ent)))
                None
         in
         let cond = Option.get cond_opt in
         ignore (Llvm.build_cond_br cond d_fast d_slow b_ent);
         let b_fast = Llvm.builder_at_end ctx d_fast in
         let kept_d_params =
           Array.to_list d_params |> List.filteri (fun i _ -> keep.(i)) |> Array.of_list
         in
         let r_fast = Llvm.build_call gspec_fty gspec_fn kept_d_params "" b_fast in
         Llvm.set_tail_call true r_fast;
         ignore (Llvm.build_ret r_fast b_fast);
         let b_slow = Llvm.builder_at_end ctx d_slow in
         if prev_fallback = fname ^ ".t0"
         then (
           let miss_ptr =
             Llvm.build_in_bounds_gep
               miss_arr_ty
               miss_glob
               [| zero; Llvm.const_int i64_ty func_id |]
               ""
               b_slow
           in
           ignore
             (Llvm.build_atomicrmw
                Llvm.AtomicRMWBinOp.Add
                miss_ptr
                one
                Llvm.AtomicOrdering.Monotonic
                false
                ""
                b_slow));
         let fb_fn = Llvm.declare_function prev_fallback orig_fty m in
         let r_slow = Llvm.build_call orig_fty fb_fn d_params "" b_slow in
         Llvm.set_tail_call true r_slow;
         ignore (Llvm.build_ret r_slow b_slow));
       (* Make orig_fn and main pure declarations before Specialize + O2 *)
       make_declaration ctx orig_fn;
       Option.iter
         (fun main_fn -> if is_definition main_fn then make_declaration ctx main_fn)
         (Llvm.lookup_function "main" m);
       (* Run static constant specialization across remaining functions *)
       ignore (Specialize.run_on_module ~is_hot:(fun _ -> true) ctx m);
       (* Set linkages so O2 can inline helper functions and $spec$ clones *)
       List.iter
         (fun fn ->
            let name = Llvm.value_name fn in
            if name = ver_name
            then Llvm.set_linkage Llvm.Linkage.External fn
            else if
              String.contains name '$'
              || (String.length name >= 5 && String.sub name 0 5 = "__ic_")
            then Llvm.set_linkage Llvm.Linkage.Internal fn
            else Llvm.set_linkage Llvm.Linkage.Available_externally fn)
         (defined_functions m);
       ignore (payload_of m);
       O2.run_on_module ~passes:"cgscc(inline)" m;
       ignore (Devirt.run_on_module ctx m);
       ignore (payload_of m);
       O2.run_on_module m;
       if Devirt.run_on_module ctx m then O2.run_on_module m;
       (* Strip any remaining Available_externally definitions so only ver_name
          (and any un-inlined Internal helpers) are emitted in the delta module *)
       List.iter
         (fun fn ->
            if Llvm.linkage fn = Llvm.Linkage.Available_externally
            then make_declaration ctx fn)
         (defined_functions m);
       let is_guarded = has_const || !used_ic in
       { master_ir with IR.payload = payload_of m }, ver_name, is_guarded)
;;

(** Per-function online state. *)
type func_state =
  | Cold
  | Optimized of
      { ver : int
      ; ver_sym : string
      ; has_ic : bool
      ; recompiles : int
      }
  | Blacklisted

let max_versions = 4
let max_recompiles = 3

(** Instruments [master_ir], runs [main] and optimizes hot functions on a
    background thread; returns [main]'s result. *)
let run_online
      (type e)
      (module E : JIT_engine_lib.JITEngine.OnlineEngine with type t = e)
      ?(hot_threshold = Tier0.default_hot_threshold)
      ?(sample_window = Tier0.default_sample_window)
      ?(timeout_ms = 15)
      ?dump_dir
      (master_ir : IR.t)
  : int64
  =
  let tier0_ir, meta = Tier0.instrument ~hot_threshold ~sample_window master_ir in
  Option.iter
    (fun dir ->
       if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
       IR.dump_to_file (Filename.concat dir "00-input.ll") master_ir;
       IR.dump_to_file (Filename.concat dir "01-tier0.ll") tier0_ir)
    dump_dir;
  let eng = E.create () in
  Fun.protect
    ~finally:(fun () -> E.close eng)
    (fun () ->
       E.load eng tier0_ir;
       let n_funcs = Array.length meta.funcs in
       E.bind_tables eng n_funcs;
       let addr_to_func : (int64, Tier0.func_info) Hashtbl.t =
         Hashtbl.create (max 8 n_funcs)
       in
       Array.iter
         (fun (info : Tier0.func_info) ->
            let addr = E.symbol_address eng info.name in
            Hashtbl.replace addr_to_func addr info)
         meta.funcs;
       ignore (E.symbol_address eng "main");
       let states = Array.make n_funcs Cold in
       let versions = Array.make n_funcs 0 in
       let recompiles = Array.make n_funcs 0 in
       let ic_disabled = Array.make n_funcs false in
       let fallbacks = Array.init n_funcs (fun id -> meta.funcs.(id).name ^ ".t0") in
       let dump_seq = ref 1 in
       let dump_delta ver_sym delta_ir =
         Option.iter
           (fun dir ->
              incr dump_seq;
              let fname =
                Filename.concat dir (Printf.sprintf "%02d-delta-%s.ll" !dump_seq ver_sym)
              in
              IR.dump_to_file fname delta_ir)
           dump_dir
       in
       let compile_and_install func_id =
         let info = meta.funcs.(func_id) in
         let ver = versions.(func_id) + 1 in
         if ver > max_versions || recompiles.(func_id) >= max_recompiles
         then (
           let delta_ir, ver_sym, _ =
             build_delta
               ~master_ir
               ~meta
               ~func_id
               ~ver
               ~prev_fallback:(info.name ^ ".t0")
               ~arg_profile:[||]
               ~site_targets:[]
           in
           dump_delta ver_sym delta_ir;
           E.load_delta eng delta_ir;
           E.install eng ~slot:func_id ~symbol:ver_sym;
           E.reset_func_profile eng ~func_id;
           versions.(func_id) <- ver;
           states.(func_id) <- Blacklisted)
         else (
           let arg_profile = E.read_arg_profile eng ~func_id ~arity:info.arity in
           let site_targets =
             if ic_disabled.(func_id)
             then []
             else
               List.filter_map
                 (fun (cs : Tier0.callsite_info) ->
                    match E.read_site_profile eng ~site_id:cs.site_id with
                    | Some (code_addr, argsc, 0) when cs.num_args = argsc ->
                      (match Hashtbl.find_opt addr_to_func code_addr with
                       | Some callee when callee.arity = argsc ->
                         Some (cs.ord_in_fn, (callee.name, argsc))
                       | _ -> None)
                    | _ -> None)
                 info.callsites
           in
           let delta_ir, ver_sym, is_guarded =
             build_delta
               ~master_ir
               ~meta
               ~func_id
               ~ver
               ~prev_fallback:fallbacks.(func_id)
               ~arg_profile
               ~site_targets
           in
           dump_delta ver_sym delta_ir;
           E.load_delta eng delta_ir;
           E.install eng ~slot:func_id ~symbol:ver_sym;
           E.reset_func_profile eng ~func_id;
           versions.(func_id) <- ver;
           if not is_guarded
           then states.(func_id) <- Blacklisted
           else (
             if Array.exists Option.is_some arg_profile
             then fallbacks.(func_id) <- ver_sym;
             states.(func_id)
             <- Optimized
                  { ver
                  ; ver_sym
                  ; has_ic = site_targets <> []
                  ; recompiles = recompiles.(func_id)
                  }))
       in
       let check_invalidations () =
         for func_id = 0 to n_funcs - 1 do
           match states.(func_id) with
           | Optimized { has_ic; recompiles = rc; _ } ->
             let _cnt, miss = E.read_counts eng ~func_id in
             if miss >= 256L
             then (
               if has_ic then ic_disabled.(func_id) <- true;
               let t0_sym = meta.funcs.(func_id).name ^ ".t0" in
               fallbacks.(func_id) <- t0_sym;
               E.reset_func_profile eng ~func_id;
               E.install eng ~slot:func_id ~symbol:t0_sym;
               recompiles.(func_id) <- rc + 1;
               states.(func_id) <- Cold)
           | Cold | Blacklisted -> ()
         done
       in
       let rec opt_loop () =
         match E.next_event eng ~timeout_ms with
         | Stop -> ()
         | Hot func_id ->
           if func_id >= 0 && func_id < n_funcs && states.(func_id) <> Blacklisted
           then (
             try compile_and_install func_id with
             | _ -> states.(func_id) <- Blacklisted);
           opt_loop ()
         | Timeout ->
           check_invalidations ();
           opt_loop ()
       in
       let opt_thread = Thread.create opt_loop () in
       Fun.protect
         ~finally:(fun () ->
           E.stop_events eng;
           Thread.join opt_thread)
         (fun () -> E.run eng "main"))
;;
