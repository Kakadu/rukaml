module Format = Stdlib.Format
open Llvm
open Miniml

let debug_log = fun x -> ()
let debug_log_action action meth name = "[" ^ meth ^ ":" ^ action ^ " {" ^ name ^ "}]"  |> debug_log


let context = Llvm.global_context ()
let builder = Llvm.builder context
let the_module = Llvm.create_module context "main"
let the_fpm = Llvm.PassManager.create_function the_module
let () = Llvm.PassManager.initialize the_fpm |> ignore

module LL = (val LL.make builder the_module)


let int_type = Llvm.i64_type context
let bool_type = Llvm.i1_type context
let unit_type = int_type
let pointer_type = Llvm.pointer_type int_type
let tuple_type = Llvm.vector_type pointer_type

let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let global_initializers:(string, Typedtree.expr) Hashtbl.t = Hashtbl.create 10
let main_body = ref None



(* TODO: More info in errors *)
type error =
  [ `NotBoundedVariable of string
  | `NotImplemented of string
  | `LambdaOutsideLet
  | `UnexpectedReturnType
  | `IncorrectFunction
  | `NotFunctionApplication
  | `ApplicationOutsideLet
  ]

let pp_error ppf: error -> _  = function
  | `NotBoundedVariable x -> Format.fprintf ppf "Variable not bounded: %s" x
  | `NotImplemented x -> Format.fprintf ppf "Not implemented: %s" x
  | `LambdaOutsideLet -> Format.fprintf ppf "Lambda outside let"
  | `UnexpectedReturnType -> Format.fprintf ppf "Ubexpected return type"
  | `IncorrectFunction -> Format.fprintf ppf "Incorrect function"
  | `NotFunctionApplication -> Format.fprintf ppf "Not function application"
  | `ApplicationOutsideLet -> Format.fprintf ppf "Application outside let"

let list_or_error lst =
  let err = List.find_opt (fun x -> Result.is_error x) lst in
  match err with
  | None -> let lst = List.map Result.get_ok lst in Result.ok lst
  | Some (Result.Error e) -> Result.error e
  | _ -> failwith "__unreachable__"

let (<<) f g x = f(g(x))
let ( let* ) x f = Result.bind x f


let validatef f =     
  (match Llvm_analysis.verify_function f with
  | true -> ()
  | false ->
      prerr_endline "invalid function generated!\n";
      prerr_endline (Llvm.string_of_llvalue f);
      prerr_endline (Llvm.string_of_llmodule the_module);
      Llvm_analysis.assert_valid_function f
  ) |> Result.ok

let transform_if_needed (v: llvalue) (t: lltype) =
  if type_of v = t then v
  else if (type_of v = int_type) && (t = pointer_type) then LL.build_inttoptr v t
  else if (type_of v = unit_type) && (t = pointer_type) then LL.build_inttoptr v t
  else if (type_of v = bool_type) && (t = pointer_type) then LL.build_inttoptr v t
  else if (type_of v = Llvm.pointer_type t) && (t <> pointer_type) && (type_of v <> pointer_type) then Llvm.build_load v "" builder 
  else Llvm.build_pointercast v t "" builder

let build_gc_alloc (t: lltype) =
  let gc_alloc = LL.lookup_func_exn "gc_alloc" in
  let size_of_elem = Llvm.size_of (Llvm.element_type t) in
  let size_of_vector = LL.const_int int_type (Llvm.vector_size t) in
  let allocate_size = LL.build_mul size_of_elem size_of_vector in
  Llvm.build_pointercast (LL.build_call gc_alloc [ allocate_size ]) (Llvm.pointer_type t) "" builder

module TypedtreeHelper = struct 
  let fold_lambda x = 
    let get_arg_type =
      function
      | Typedtree.Arrow (x, _) -> x.typ_desc
      | _ -> failwith "fail lambda"
    in
    let rec helper lambda args =
      match lambda with
      | Typedtree.TLam (name, e, t) ->
        let t = get_arg_type t.typ_desc in
        helper e ((name, t) :: args) 
      | x -> List.rev args, x
    in helper x []

  let fold_arrow x = 
    let rec helper arrow args =
      match arrow with
      | Typedtree.Arrow  (x, y) -> helper y.typ_desc (x.typ_desc:: args) 
      | x -> List.rev args, x
    in helper x []

  let fold_application x =
    let rec helper application args =
      match application with
      | Typedtree.TApp (f, e, _) -> helper f (e :: args)
      | x -> args, x
    in helper x []
end

module Builtins = struct

  let builtin_binops = [
    ("+", LL.build_add, int_type);
    ("*", LL.build_mul, int_type);
    ("-", LL.build_sub, int_type);
    ("=", LL.build_eq, int_type)
  ]
  let is_builtin_binop n = List.exists (fun (op_n, _, _) -> op_n = n ) builtin_binops
  let call_builtin_binop n arg1 arg2 = 
    let (_, op, op_type) = List.find ( fun (op_n, _, _) -> op_n = n ) builtin_binops in
    let arg1 = transform_if_needed arg1 op_type in
    let arg2 = transform_if_needed arg2 op_type in
    op arg1 arg2

  let build_tuple_get name index size = 
    let function_type = Llvm.function_type pointer_type [| Llvm.pointer_type (tuple_type size) |] in
    let f = LL.declare_function name function_type in
    let basic_block = Llvm.append_block context "entry" f in
    LL.position_at_end basic_block;

    let tuple = LL.build_load (param f 0) in
    let ret_val = (Llvm.build_extractelement tuple (LL.const_int int_type index) "" builder) in
    LL.build_ret ret_val |> ignore;
  ;;

  let declare_builtins () =
    LL.declare_function "print_int" (Llvm.function_type unit_type [| int_type |]) |> ignore;
    LL.declare_function "print_bool" (Llvm.function_type unit_type [| bool_type |]) |> ignore;
    LL.declare_function "trace_int" (Llvm.function_type int_type [| int_type |]) |> ignore;
    LL.declare_function "trace_bool" (Llvm.function_type bool_type [| bool_type |]) |> ignore;
    LL.declare_function "get_int_arg" (Llvm.function_type int_type [| int_type |]) |> ignore;
    LL.declare_function "applyN" (Llvm.var_arg_function_type pointer_type [| pointer_type; int_type |]) |> ignore;
    LL.declare_function "alloc_closure" (Llvm.function_type pointer_type [| pointer_type; int_type|]) |> ignore;
    LL.declare_function "gc_alloc" (Llvm.function_type pointer_type [| int_type |]) |> ignore;
    build_tuple_get "fst" 0 2;
    build_tuple_get "snd" 1 2;
end

module LlvmFunction = struct
  let rec to_llvm_type =
    function
    | Typedtree.Prim "int" ->  int_type
    | Typedtree.Prim "bool" -> bool_type
    | Typedtree.Prim "unit" -> unit_type
    | Typedtree.V _ -> pointer_type
    | Typedtree.Arrow _ as arr -> 
      let (args, ret_type) = TypedtreeHelper.fold_arrow arr
      in Llvm.function_type (to_llvm_type ret_type) (List.map to_llvm_type args |> Array.of_list) |> Llvm.pointer_type
    | Typedtree.Prim x -> failwith ("Unexpected primitive " ^ x)
    | Typedtree.TLink _ -> failwith "Unexpected link"
    | Typedtree.TProd (_, _, lst) -> tuple_type (2 + List.length lst) |> Llvm.pointer_type

  let get_llvm_args = List.map (to_llvm_type << snd)

  

  let build_f name args ret_type ret_val_builder =
    let args_names = List.map fst args in
    let args_types = get_llvm_args args |> Array.of_list in 

    let function_type = Llvm.function_type ret_type args_types in
    let f = LL.declare_function name function_type in
    (* set_gc (Some "shadow-stack") f; *)
    let basic_block = Llvm.append_block context "entry" f in
    LL.position_at_end basic_block;

    Array.iter (fun (arg, arg_name) ->
      set_value_name arg_name arg;
      Hashtbl.add named_values arg_name arg;
    ) (Array.combine (params f) (Array.of_list args_names));

    let* ret_val = ret_val_builder f in
    let ret_val = transform_if_needed ret_val ret_type in
    LL.build_ret ret_val |> ignore;

    List.iter (Hashtbl.remove named_values) args_names;

    validatef f

  let build_function name args body codegen =
    let ret_val_builder = (fun _ -> codegen body) in
    let ret_type = (Typedtree.type_of_expr body).typ_desc |> to_llvm_type in
    build_f name args ret_type ret_val_builder

  let build_init_globals codegen =
    let ret_val_builder = fun _ ->
      let globs = Hashtbl.to_seq global_initializers in
      let init_globs = globs |> Seq.map (function (name, init) -> 
        let* value = codegen init in
        let glob = Hashtbl.find named_values name in
        Llvm.build_store value glob builder |> ignore |> Result.ok
      ) in

      let* _ = init_globs |> List.of_seq |> list_or_error in
      Llvm.const_int int_type 1 |> Result.ok
    in
    build_f "init_globals" [ ] unit_type ret_val_builder

  let build_main codegen =
    let* _ = build_init_globals codegen in
    let ret_val_builder = fun f ->
      Hashtbl.add named_values "main" f;
      LL.build_call (LL.lookup_func_exn "init_globals") [] |> ignore;
      codegen (Option.get main_body.contents)
    in
    build_f "real_main" [ ] int_type ret_val_builder

  let build_if cond then_ else_ codegen =
      let* cond = codegen cond in

      let zero = const_int bool_type 0 in
      let cond_val = build_icmp Icmp.Ne cond zero "ifcond" builder in

      let start_bb = insertion_block builder in
      let the_function = block_parent start_bb in

      let then_bb = append_block context "then" the_function in

      position_at_end then_bb builder;
      let* then_val = codegen then_ in

      let new_then_bb = insertion_block builder in

      let else_bb = append_block context "else" the_function in
      position_at_end else_bb builder;
      let* else_val = codegen else_ in

      let new_else_bb = insertion_block builder in

      let merge_bb = append_block context "ifcont" the_function in
      position_at_end merge_bb builder;
      let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
      let phi = build_phi incoming "iftmp" builder in

      position_at_end start_bb builder;
      ignore (build_cond_br cond_val then_bb else_bb builder);

      position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
      position_at_end new_else_bb builder; ignore (build_br merge_bb builder);

      position_at_end merge_bb builder;

      phi |> Result.ok

  let alloc_closure f expected_args_count = 
    let alloc_closure = LL.lookup_func_exn "alloc_closure" in
    let f_pointer = transform_if_needed f pointer_type in
    LL.build_call alloc_closure [f_pointer; LL.const_int int_type expected_args_count]

  let alloc_closure_if_needed v =
    match (classify_value v) with
    | Function -> alloc_closure v (params v |> Array.length)
    | _ -> v

  let build_direct_call f args =
    let prepare_arg v t =
      transform_if_needed (alloc_closure_if_needed v) t

    in
    let real_args = 
      Array.combine (params f) (Array.of_list args) 
      |> Array.map (fun (p, v) -> (Llvm.type_of p, v))
      |> Array.map (fun (t, v) -> prepare_arg v t)
      |> Array.to_list
    in
    LL.build_call f real_args |> Result.ok

  let build_partial_application f args expected_args_count real_args_count = 
    let callee = alloc_closure f expected_args_count in
    let applyN = LL.lookup_func_exn "applyN" in
    let final_args = callee :: LL.const_int int_type real_args_count :: (List.map alloc_closure_if_needed args) in
    LL.build_call applyN final_args |> Result.ok

  let build_closure_call f args = 
    let arg_number = List.length args in
    let applyN = LL.lookup_func_exn "applyN" in
    let callee_f = 
      match (classify_value f) with
      | GlobalVariable -> Llvm.build_load f "" builder
      | _ -> transform_if_needed f pointer_type
    in
    let final_args = callee_f :: LL.const_int int_type arg_number :: (List.map alloc_closure_if_needed args) in
    LL.build_call applyN final_args |> Result.ok

  let build_call name args codegen =
    let* args = List.map codegen args |> list_or_error in
    if Builtins.is_builtin_binop name then
      match args with
      | [arg1; arg2] -> Builtins.call_builtin_binop name arg1 arg2 |> Result.ok
      | _ ->  failwith "Unreachable"
    else
      let real_args_count = args |> List.length in
      match (Hashtbl.find_opt named_values name) with
      | None -> 
        let calling_f = LL.lookup_func_exn name in
        let expected_args_count = Array.length (LL.params calling_f) in
        (match (expected_args_count - real_args_count) with
        | 0 -> build_direct_call calling_f args
        | _ -> build_partial_application calling_f args expected_args_count real_args_count)
      | Some f -> build_closure_call f args
end

module LlvmTuple = struct

  let create_tuple args codegen = 
    let* values = List.map codegen args |> list_or_error in
    let values = List.map (fun v -> transform_if_needed v pointer_type) values in
    let size = List.length values in
    let array = build_gc_alloc (tuple_type size) in
    let i = ref (-1) in
    let result = List.fold_left (fun vec v -> i := (i.contents + 1); build_insertelement vec v (LL.const_int (i32_type context) i.contents) "" builder) (LL.build_load array) values in
    Llvm.build_store result array builder |> ignore;
    array |> Result.ok
end

let rec codegen = 
  function
  | Typedtree.TConst x -> 
      (match x with
      | PConst_int i -> LL.const_int int_type i |> Result.ok
      | PConst_bool b ->  LL.const_bool int_type b |> Result.ok)

  | Typedtree.TVar (name, _) ->
      (match Hashtbl.find_opt named_values name with 
        | None -> LL.lookup_func_exn name |> Result.ok
        | Some x -> x |> Result.ok)

  | Typedtree.TIf (c, t, e, _) -> LlvmFunction.build_if c t e codegen

  | Typedtree.TLet (_, pattern, _, value_expr, in_expr) -> 
      let* value = codegen value_expr in
      Hashtbl.add named_values pattern value;
      codegen in_expr

  | Typedtree.TApp _ as app ->
      let (args, f) = TypedtreeHelper.fold_application app in
      (match f with
      | Typedtree.TVar (name, _) -> LlvmFunction.build_call name args codegen
      | _ -> Result.error `NotFunctionApplication) 

  | Typedtree.TLam _ -> Result.error `LambdaOutsideLet

  | Typedtree.TUnit -> LL.const_int int_type 0 |> Result.ok

  | Typedtree.TTuple (fst, snd, other, _) -> LlvmTuple.create_tuple (fst :: snd :: other) codegen


let codegen_top_level =
  function
  | Typedtree.TLet (_, "main", _, value_expr, _) ->
      (main_body := Some value_expr) |> Result.ok

  | Typedtree.TLet (_, name, _, (Typedtree.TLam _ as lambda) , in_expr) ->
      let (args, body) = TypedtreeHelper.fold_lambda lambda in
      let _ = LlvmFunction.build_function name args body codegen in
      codegen in_expr |> ignore |> Result.ok

  | Typedtree.TLet (_, name, _, value_expr, in_expr) ->
      let global_value = Llvm.define_global name (Llvm.const_null pointer_type) the_module in
      Hashtbl.add global_initializers name value_expr;
      Hashtbl.add named_values name global_value;
      codegen in_expr |> ignore |> Result.ok

  | expr -> "incorrect toplevel expr: " ^ (Typedtree.show_expr expr) |> failwith

let dump_to_object ~the_fpm filename =
  Llvm_all_backends.initialize ();

  let target_triple = Llvm_target.Target.default_triple () in
  let target = Llvm_target.Target.by_triple target_triple in
  let cpu = "generic" in
  let reloc_mode = Llvm_target.RelocMode.Default in
  let machine =
    Llvm_target.TargetMachine.create ~triple:target_triple ~cpu ~reloc_mode
      target
  in

  let data_layout =
    Llvm_target.TargetMachine.data_layout machine
    |> Llvm_target.DataLayout.as_string
  in
  Llvm.set_target_triple target_triple the_module;
  Llvm.set_data_layout data_layout the_module;
  Llvm_target.TargetMachine.add_analysis_passes the_fpm machine;
  let file_type = Llvm_target.CodeGenFileType.ObjectFile in

  Llvm_target.TargetMachine.emit_to_file the_module file_type filename machine

let dump emit_llvm object_out llvm_out =
  dump_to_object ~the_fpm object_out;
  if emit_llvm then Llvm.print_module llvm_out the_module

let value_binding_to_expr (x: Typedtree.value_binding) =
  let name_of_pattern (Parsetree.PVar s) = s in
  let name = name_of_pattern x.tvb_pat in
  Typedtree.TLet (x.tvb_flag, name, x.tvb_typ, x.tvb_body, Typedtree.TVar (name, Typedtree.type_of_expr x.tvb_body ))
  |> Typedtree.compact_expr

let generate_llvm (value_bindings: Typedtree.value_binding list) emit_llvm llvm_out object_out = 
  Builtins.declare_builtins ();
  let result =
    let* _ = List.map (codegen_top_level << value_binding_to_expr) value_bindings |> list_or_error in
    let* _ = LlvmFunction.build_main codegen in
    () |> Result.ok
  in
  match result with
  | Result.Ok _ -> dump emit_llvm object_out llvm_out |> Result.ok
  | Result.Error e -> e |> Result.error