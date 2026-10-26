open Common

let name = "tier0"
let requires = [ "rukaml_jit_hot"; "rukaml_prof_args"; "rukaml_prof_site" ]
let default_hot_threshold = 256L
let default_sample_window = 64L

type callsite_info =
  { site_id : int
  ; func_id : int
  ; ord_in_fn : int
  ; num_args : int
  }

type func_info =
  { id : int
  ; name : string
  ; arity : int
  ; callsites : callsite_info list
  }

type meta =
  { funcs : func_info array
  ; num_sites : int
  ; hot_threshold : int64
  ; sample_window : int64
  }

let slotted_functions m =
  defined_functions m
  |> List.filter (fun fn ->
    let name = Llvm.value_name fn in
    let arity = Array.length (Llvm.params fn) in
    name <> "main" && arity >= 1 && arity <= 4)
;;

let instrument_module
      ?(hot_threshold = default_hot_threshold)
      ?(sample_window = default_sample_window)
      ctx
      m
  : meta
  =
  let i64_ty = Llvm.i64_type ctx in
  let ptr_ty = Llvm.pointer_type ctx in
  let void_ty = Llvm.void_type ctx in
  let zero = Llvm.const_int i64_ty 0 in
  let one = Llvm.const_int i64_ty 1 in
  let hot_const = Llvm.const_of_int64 i64_ty hot_threshold true in
  let win_const = Llvm.const_of_int64 i64_ty sample_window true in
  let hot_ty = Llvm.function_type void_ty [| i64_ty |] in
  let prof_args_ty =
    Llvm.function_type void_ty [| i64_ty; i64_ty; i64_ty; i64_ty; i64_ty |]
  in
  let prof_site_ty = Llvm.function_type void_ty [| i64_ty; i64_ty |] in
  let hot_fn = Llvm.declare_function "rukaml_jit_hot" hot_ty m in
  let prof_args_fn = Llvm.declare_function "rukaml_prof_args" prof_args_ty m in
  let prof_site_fn = Llvm.declare_function "rukaml_prof_site" prof_site_ty m in
  let slotted = slotted_functions m in
  let n_funcs = List.length slotted in
  let n_slots = max 1 n_funcs in
  let slots_arr_ty = Llvm.array_type ptr_ty n_slots in
  let cnt_arr_ty = Llvm.array_type i64_ty n_slots in
  let miss_arr_ty = Llvm.array_type i64_ty n_slots in
  let next_site_id = ref 0 in
  let func_entries =
    List.mapi
      (fun id fn ->
         let fname = Llvm.value_name fn in
         let params = Llvm.params fn in
         let arity = Array.length params in
         let ret_ty = Option.value (ret_type_of ctx fn) ~default:i64_ty in
         let fty = Llvm.function_type ret_ty (Array.map Llvm.type_of params) in
         let dsites = dyn_sites_of_fn fn in
         let callsites =
           List.mapi
             (fun ord (ds : dyn_site) ->
                let sid = !next_site_id in
                incr next_site_id;
                { site_id = sid
                ; func_id = id
                ; ord_in_fn = ord
                ; num_args = Array.length ds.args
                })
             dsites
         in
         let keep = Array.make arity true in
         let t0_name = fname ^ ".t0" in
         (* Trivial one to one cloning, all params are ketp *)
         let t0_fn =
           match clone_specialized ctx m ~name:t0_name ~fty ~keep ~args:[||] fn with
           | Some f' -> f'
           | None -> failwith ("tier0: failed to clone " ^ fname)
         in
         let info = { id; name = fname; arity; callsites } in
         info, fn, fty, t0_fn)
      slotted
  in
  let init_ptrs =
    if n_funcs = 0
    then [| Llvm.const_null ptr_ty |]
    else Array.of_list (List.map (fun (_, _, _, t0_fn) -> t0_fn) func_entries)
  in
  let slots_glob =
    Llvm.define_global "rukaml.slots" (Llvm.const_array ptr_ty init_ptrs) m
  in
  let cnt_glob = Llvm.define_global "rukaml.cnt" (Llvm.const_null cnt_arr_ty) m in
  let miss_glob = Llvm.define_global "rukaml.miss" (Llvm.const_null miss_arr_ty) m in
  Llvm.set_alignment 8 slots_glob;
  Llvm.set_alignment 8 cnt_glob;
  Llvm.set_alignment 8 miss_glob;
  List.iter
    (fun (info, fn, fty, t0_fn) ->
       let const_id = Llvm.const_int i64_ty info.id in
       (* 1. Instrument dynamic applyN sites in t0_fn *)
       let dsites_t0 = dyn_sites_of_fn t0_fn in
       List.iter2
         (fun cs (ds : dyn_site) ->
            let head_ins =
              match ds.intermediates with
              | hd :: _ -> hd
              | [] -> ds.ins
            in
            let b = Llvm.builder ctx in
            Llvm.position_before head_ins b;
            let sid_const = Llvm.const_int i64_ty cs.site_id in
            ignore
              (Llvm.build_call prof_site_ty prof_site_fn [| sid_const; ds.root_cl |] "" b))
         info.callsites
         dsites_t0;
       (* 2. Insert profiling prologue blocks before t0_fn's entry block *)
       let body_entry = Llvm.entry_block t0_fn in
       let t0_entry = Llvm.insert_block ctx "t0_entry" body_entry in
       let t0_sample = Llvm.insert_block ctx "t0_sample" body_entry in
       let t0_check_hot = Llvm.insert_block ctx "t0_check_hot" body_entry in
       let t0_hot = Llvm.insert_block ctx "t0_hot" body_entry in
       let b = Llvm.builder_at_end ctx t0_entry in
       let cnt_ptr =
         Llvm.build_in_bounds_gep cnt_arr_ty cnt_glob [| zero; const_id |] "" b
       in
       let old_cnt = Llvm.build_load i64_ty cnt_ptr "" b in
       let new_cnt = Llvm.build_add old_cnt one "" b in
       ignore (Llvm.build_store new_cnt cnt_ptr b);
       let in_win = Llvm.build_icmp Llvm.Icmp.Ule new_cnt win_const "" b in
       ignore (Llvm.build_cond_br in_win t0_sample t0_check_hot b);
       let b_samp = Llvm.builder_at_end ctx t0_sample in
       let p idx = if idx < info.arity then Llvm.param t0_fn idx else zero in
       ignore
         (Llvm.build_call
            prof_args_ty
            prof_args_fn
            [| const_id; p 0; p 1; p 2; p 3 |]
            ""
            b_samp);
       ignore (Llvm.build_br t0_check_hot b_samp);
       let b_chk = Llvm.builder_at_end ctx t0_check_hot in
       let is_hot = Llvm.build_icmp Llvm.Icmp.Eq new_cnt hot_const "" b_chk in
       ignore (Llvm.build_cond_br is_hot t0_hot body_entry b_chk);
       let b_hot = Llvm.builder_at_end ctx t0_hot in
       ignore (Llvm.build_call hot_ty hot_fn [| const_id |] "" b_hot);
       ignore (Llvm.build_br body_entry b_hot);
       (* 3. Turn original fn into a slot-dispatching stub *)
       clear_function_body ctx fn;
       let stub_bb = Llvm.append_block ctx "entry" fn in
       let b_stub = Llvm.builder_at_end ctx stub_bb in
       let slot_ptr =
         Llvm.build_in_bounds_gep slots_arr_ty slots_glob [| zero; const_id |] "" b_stub
       in
       let target_ptr = Llvm.build_load ptr_ty slot_ptr "" b_stub in
       Llvm.set_alignment 8 target_ptr;
       let call = Llvm.build_call fty target_ptr (Llvm.params fn) "" b_stub in
       Llvm.set_tail_call true call;
       ignore (Llvm.build_ret call b_stub))
    func_entries;
  let funcs = Array.of_list (List.map (fun (info, _, _, _) -> info) func_entries) in
  { funcs; num_sites = !next_site_id; hot_threshold; sample_window }
;;

let instrument
      ?(hot_threshold = default_hot_threshold)
      ?(sample_window = default_sample_window)
      (ir : IR.t)
  : IR.t * meta
  =
  let ctx, m = parse_payload ir in
  Fun.protect
    ~finally:(fun () ->
      Llvm.dispose_module m;
      Llvm.dispose_context ctx)
    (fun () ->
       let meta = instrument_module ~hot_threshold ~sample_window ctx m in
       { ir with IR.payload = payload_of m }, meta)
;;

let run (ir : IR.t) : IR.t = fst (instrument ir)
