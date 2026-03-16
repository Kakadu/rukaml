open Compile_lib.ANF
open Irml
open Bindings

type value =
  | Addr of unit Ctypes_static.ptr
  | LocalNode of int32

module SMap = Map.Make (String)

let size_buf () = Ctypes.allocate Ctypes.ulong Unsigned.ULong.zero

module Code_buf : sig
  type t
  val create : int -> t
  val add_thunk : t -> unit Ctypes_static.ptr * Unsigned.ULong.t
  val fix_thunk : t -> unit Ctypes_static.ptr -> unit Ctypes_static.ptr -> unit
end = struct
  (* buf ptr, start ptr, size *)
  type t =
    (code_buffer, [ `Struct ]) Ctypes.structured Ctypes.ptr
    * unit Ctypes.ptr
    * Unsigned.ULong.t
  let create size =
    let open Ctypes in
    let buf = make code_buffer in
    let sz_sizet = Unsigned.ULong.of_int size in
    let st =
      (*TODO: err hndl?*)
      ir_mem_mmap sz_sizet
    in
    setf buf start st;
    setf buf endd @@ to_voidp (from_voidp (ptr char) st +@ size);
    setf buf pos st;
    addr buf, st, sz_sizet
  ;;

  let add_thunk (buf, st, size) =
    let open Ctypes in
    let sz_buf = size_buf () in
    (*TODO: err hndl?*)
    ignore @@ ir_mem_unprotect st size;
    (*TODO: err hndl?*)
    let thunk = ir_emit_thunk buf null sz_buf in
    (*TODO: err hndl?*)
    ignore @@ ir_mem_protect st size;
    thunk, !@sz_buf
  ;;

  let fix_thunk (_, st, size) th new_addr =
    (*TODO: err hndl?*)
    ignore @@ ir_mem_unprotect st size;
    ir_fix_thunk th new_addr;
    (*TODO: err hndl?*)
    ignore @@ ir_mem_protect st size
  ;;
end

let to_son env ctx pats e =
  let start = ir_emit0 ctx Consts.ir_start in
  let _, env =
    List.fold_left
      (fun (i, env) (APname { hum_name = name; _ }) ->
         let n = LocalNode (ir_param ctx Consts.ir_i64 start name i) in
         i + 1, SMap.add name n env)
      (1, env)
      pats
  in
  let rec helper e env ctrl =
    match (e : Compile_lib.ANF.expr) with
    | ELet (_, Tpat_var { hum_name = name; _ }, c, e) ->
      let ctrl, data = helper_c c env ctrl in
      helper e (SMap.add name (LocalNode data) env) ctrl
    | EComplex c -> helper_c c env ctrl
    | ELet _ -> assert false
  and helper_c c env ctrl =
    match c with
    | CIte (c, e1, e2) ->
      let ctrl, data = helper_c c env ctrl in
      let cond = ir_emit2 ctx Consts.ir_if ctrl data in
      let ctrl1, data1 = helper e1 env @@ ir_emit1 ctx Consts.ir_if_true cond in
      let ctrl2, data2 = helper e2 env @@ ir_emit1 ctx Consts.ir_if_false cond in
      let merge_p =
        ir_emit2
          ctx
          Consts.ir_merge2
          (ir_emit1 ctx Consts.ir_end ctrl1)
          (ir_emit1 ctx Consts.ir_end ctrl2)
      in
      merge_p, ir_emit3 ctx Consts.ir_phi2_i64 merge_p data1 data2
    | CApp (APrimitive b, i1, [ i2 ]) when is_infix_binop b ->
      let op =
        let open Consts in
        match b with
        | "=" -> ir_eq
        | "+" -> ir_add_i64
        | "-" -> ir_sub_i64
        | "*" -> ir_mul_i64
        | "/" -> ir_div_i64
        | "<" -> ir_lt
        | "<=" -> ir_le
        | ">" -> ir_gt
        | ">=" -> ir_ge
        | _ -> failwith "new binop?"
      in
      ctrl, ir_fold2 ctx op (helper_imm env i1) (helper_imm env i2)
    | CApp (APrimitive _, _, _) -> failwith "not impl yet"
    | CApp (i0, i1, ii) ->
      let f = helper_imm env i0 in
      let a1 = helper_imm env i1 in
      let aa = List.map (helper_imm env) ii in
      let clone x = x, x in
      clone
        (match aa with
         | [] -> ir_emit3 ctx Consts.ir_call1_i64 ctrl f a1
         | _ ->
           let len = List.length aa in
           let call = ir_emitN ctx Consts.ir_call_i64 @@ Int32.of_int (len + 3) in
           ir_set_op ctx call 1l ctrl;
           ir_set_op ctx call 2l f;
           ir_set_op ctx call 3l a1;
           List.iteri (fun pos a -> ir_set_op ctx call (Int32.of_int (pos + 4)) a) aa;
           call)
    | CAtom imm -> ctrl, helper_imm env imm
  and helper_imm env imm =
    let const n = ir_fold1 ctx Consts.ir_copy_i64 @@ ir_const_i64 ctx @@ Int64.of_int n in
    match imm with
    | AUnit | AConst (PConst_bool false) -> const 0
    | AConst (PConst_bool true) -> const 1
    | AConst (PConst_int n) -> const n
    | AVar { hum_name = name; _ } ->
      (match SMap.find name env with
       | Addr a -> ir_const_addr_w ctx a
       | LocalNode n -> n)
    | _ -> failwith ""
  in
  let ctrl, data = helper e env start in
  let ret = ir_emit2 ctx Consts.ir_return ctrl data in
  ir_set_op ctx start 1l ret
;;

let compile_vb env name b =
  let pats, e = Compile_lib.ANF.group_abstractions b in
  let argc = List.length pats in
  assert (argc >= 1 || name = "main");
  let ctx = ir_create_ctx () in
  ir_consistency_check ();
  let flags = Unsigned.UInt32.add Consts.ir_function Consts.ir_opt_folding in
  (* init sizes for due buffers*)
  let consts, insns = 16l, 48l in
  ir_init ctx flags consts insns;
  to_son env ctx pats e;
  ir_build_def_use_lists ctx;
  let _ = ir_sccp ctx in
  (*todo : check if nonzero?*)
  ir_build_cfg ctx;
  ir_build_dominators_tree ctx;
  ir_find_loops ctx;
  ir_gcm ctx;
  ir_schedule ctx;
  ir_match ctx;
  ir_assign_virtual_registers ctx;
  ir_compute_live_ranges ctx;
  ir_coalesce ctx;
  ir_reg_alloc ctx;
  ir_schedule_blocks ctx;
  (* Maybe later we'd like to add: ir_mem2ssa *)
  let sz_buf = size_buf () in
  let entry = ir_emit_code ctx sz_buf in
  entry, ctx, Ctypes.( !@ ) sz_buf
;;

let codegen vbs out_file =
  let s_out = fopen out_file "w" in
  let open Code_buf in
  let code_buf =
    create 10240
    (*10 KB for thunks*)
  in
  let add_externals =
    List.fold_left
    @@ fun env (asm_name, name) ->
    (*TODO: err hndl?*)
    ignore @@ fprintf s_out (".extern " ^ asm_name ^ "\n");
    (* (thunk) never be fixed *)
    let th, sz = add_thunk code_buf in
    ir_disasm_add_symbol_w asm_name th sz;
    SMap.add name (Addr th) env
  in
  let init_env = add_externals SMap.empty [ "rukaml_print_int", "print" ] in
  (*TODO: err hndl?*)
  ignore @@ fprintf s_out ".global main\n\n";
  let fin (name, entry, ctx, size) =
    ignore @@ ir_disasm name entry size false ctx s_out;
    ir_free ctx
  in
  List.iter fin
  @@ snd
  @@ List.fold_left_map
       (fun glob_env -> function
          | Frontend.Parsetree.Recursive, ({ hum_name = name; _ } : Frontend.Ident.t), b
            ->
            let th, sz = add_thunk code_buf in
            ir_disasm_add_symbol_w name th sz;
            let glob_env' = SMap.add name (Addr th) glob_env in
            let entry, ctx, size = compile_vb glob_env' name b in
            Code_buf.fix_thunk code_buf th entry;
            ir_disasm_add_symbol_w name entry size;
            SMap.add name (Addr entry) glob_env, (name, entry, ctx, size)
          | NonRecursive, { hum_name = name; _ }, b ->
            let entry, ctx, size = compile_vb glob_env name b in
            ir_disasm_add_symbol_w name entry size;
            SMap.add name (Addr entry) glob_env, (name, entry, ctx, size))
       init_env
       vbs;

  ignore @@ fclose s_out
;;
