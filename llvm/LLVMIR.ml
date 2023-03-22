
module Format = Stdlib.Format
open Llvm
open Miniml

let debug_log = prerr_endline
let debug_log_action action meth name = "[" ^ meth ^ ":" ^ action ^ " {" ^ name ^ "}]"  |> debug_log


let context = Llvm.global_context ()
let builder = Llvm.builder context
let int_type = Llvm.i64_type context
let bool_type = int_type
let unit_type = int_type
let tuple_type = Llvm.array_type int_type
let pointer_type = Llvm.pointer_type int_type


let the_module = Llvm.create_module context "main"
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let global_initializers:(string, lltype * Typedtree.expr) Hashtbl.t = Hashtbl.create 10
let main_body = ref None

let the_fpm = Llvm.PassManager.create_function the_module
let () = Llvm.PassManager.initialize the_fpm |> ignore

module LL = (val LL.make builder the_module)

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

module TypedtreeHelper = struct 
  let fold_lambda x = 
    let get_arg_type =
      function
      | Typedtree.Arrow (x, _) -> x
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

  let substitute name value f =
    let rec helper = function
    | Typedtree.TVar (x, _) as v -> if name = x then value else v 
    | Typedtree.TLet (r, n, s, e1, e2) -> Typedtree.TLet (r, n, s, helper e1, helper e2)
    | Typedtree.TApp (e1, e2, t) -> Typedtree.TApp (helper e1, helper e2, t)
    | Typedtree.TLam (p, e, t) -> Typedtree.TLam (p, helper e, t) (* TODO: Fix me *)
    | x -> x
  in helper f 
end

module Builtins = struct

  let builtin_binops = [
    ("+", LL.build_add);
    ("*", LL.build_mul);
    ("-", LL.build_sub)
  ]
  let is_builtin_binop n = List.exists ( (=) n << fst) builtin_binops
  let call_builtin_binop n arg1 arg2 = 
    let op = List.find ( (=) n << fst) builtin_binops |> snd in
    op arg1 arg2

  let declare_builtins () =
    LL.declare_function "print_int" (Llvm.function_type unit_type [| int_type |]) |> ignore;
    LL.declare_function "print_bool" (Llvm.function_type unit_type [| bool_type |]) |> ignore;
    LL.declare_function "trace_int" (Llvm.function_type int_type [| int_type |]) |> ignore;
    LL.declare_function "trace_bool" (Llvm.function_type bool_type [| bool_type |]) |> ignore;
    LL.declare_function "get_int_arg" (Llvm.function_type int_type [| int_type |]) |> ignore;

    LL.declare_function "applyN" (Llvm.var_arg_function_type pointer_type [| pointer_type; int_type |]) |> ignore;
    LL.declare_function "alloc_closure" (Llvm.function_type pointer_type [| pointer_type; int_type|]) |> ignore;
end

module LlvmFunction = struct
  let is_prim_arg (t: Typedtree.ty) =
    (* Typedtree.pp_ty Format.std_formatter t; *)
    match t.typ_desc with
    | Typedtree.Prim "int" | Typedtree.Prim "bool" | Typedtree.Prim "unit" -> true
    | _ -> false

  let to_llvm_prim_type (t: Typedtree.type_desc) =
    match t with
    | Typedtree.Prim "int" ->  int_type
    | Typedtree.Prim "bool" -> bool_type
    | Typedtree.Prim "unit" -> unit_type
    | _ -> failwith "fail prim"

  let to_llvm_type (t: Typedtree.ty) =
    match t.typ_desc with
    | Typedtree.Prim "int" ->  int_type
    | Typedtree.Prim "bool" -> bool_type
    | Typedtree.Prim "unit" -> unit_type
    | Typedtree.Arrow _ as arr -> 
      let (args, ret_type) = TypedtreeHelper.fold_arrow arr
      in Llvm.function_type (to_llvm_prim_type ret_type) (List.map to_llvm_prim_type args |> Array.of_list) |> Llvm.pointer_type
    | _ -> failwith "fail arg"

  let get_value (v: llvalue) (t: lltype) =
    if type_of v = t then v
    else Llvm.build_load v "" builder
  let has_lambda_arg = List.exists (not << is_prim_arg)
  let get_llvm_args = List.map (to_llvm_type << snd)

  let build_function name args body codegen =
    debug_log_action "start" "build_function" name;
    let args_names = List.map fst args in
    let args_types = get_llvm_args args |> Array.of_list in 

    let function_type = Llvm.function_type int_type args_types in
    let f = LL.declare_function name function_type in
    let basic_block = Llvm.append_block context "entry" f in
    LL.position_at_end basic_block;
    Hashtbl.add named_values name f;

    Array.iter (fun (arg, arg_name) ->
      set_value_name arg_name arg;
      Hashtbl.add named_values arg_name arg;
    ) (Array.combine (params f) (Array.of_list args_names));

    let* ret_val = codegen body in
    LL.build_ret ret_val |> ignore;

    debug_log_action "finished" "build_function" name;
    debug_log (string_of_llvalue f);

    validatef f

  let build_init_globals codegen = 
    debug_log_action "start" "build_init_globals" "";

    let function_type = Llvm.function_type int_type [| |] in
    let f = LL.declare_function "init_globals" function_type in
    let basic_block = Llvm.append_block context "entry" f in
    LL.position_at_end basic_block;
    Hashtbl.add named_values "init_globals" f;

    let globs = Hashtbl.to_seq global_initializers in

    let kek = globs |> Seq.map (function (name, (t, init)) -> 
      let* value = codegen init in
      let glob = Hashtbl.find named_values name in
      let value' = LL.build_pointercast value t in
      Llvm.build_store value' glob builder |> ignore |> Result.ok
    ) in

    let* _ = kek |> List.of_seq |> list_or_error in

    (Llvm.const_int int_type 1) |> LL.build_ret |> ignore;

    debug_log (string_of_llmodule the_module);
    debug_log_action "finished" "build_init_globals" "" ;
    (* debug_log (string_of_llvalue f);
    debug_log "1"; *)

    validatef f

  let build_main codegen = 
    debug_log_action "start" "build_main" "";

    let* _ = build_init_globals codegen in

    let function_type = Llvm.function_type int_type [||] in
    let f = LL.declare_function "real_main" function_type in
    let basic_block = Llvm.append_block context "entry" f in
    LL.position_at_end basic_block;
    Hashtbl.add named_values "main" f;

    LL.build_call (LL.lookup_func_exn "init_globals") [] |> ignore;
    let* ret_val = codegen (Option.get main_body.contents) in
    LL.build_ret ret_val |> ignore;

    debug_log_action "finished" "build_function" "";
    debug_log (string_of_llvalue f);

    validatef f

  let build_call name args codegen =
    debug_log_action "start" "build_call" name;
    let* args = List.map codegen args |> list_or_error in
    if Builtins.is_builtin_binop name then
      match args with
      | [arg1; arg2] ->
        debug_log_action "finished(builtin)" "build_call" name;
        Builtins.call_builtin_binop name arg1 arg2 |> Result.ok
      | _ ->  failwith "Unreachable"
    else
      let calling_f = LL.lookup_func_exn name 
        (* match (Hashtbl.find_opt named_values name) with
        | None -> LL.lookup_func_exn name 
        | Some f -> f *)
      in

      let real_args_count = args |> List.length in
      let expected_args_count = Array.length (LL.params calling_f) in
      (match (expected_args_count - real_args_count) with
      | 0 ->
        let real_args = 
          Array.combine (params calling_f) (Array.of_list args) 
          |> Array.map (fun (p, x) -> (Llvm.type_of p, x))
          |> Array.map (fun (t, v) -> get_value v t)
          |> Array.to_list  
        in
        debug_log_action "finished(direct call)" "build_call" name;
        LL.build_call calling_f real_args |> Result.ok
      | _ -> 
        let callee = 
          let alloc_closure = LL.lookup_func_exn "alloc_closure" in
          let f_pointer = LL.build_pointercast calling_f pointer_type in
          LL.build_call alloc_closure [f_pointer; LL.const_int int_type expected_args_count]
        in
        let applyN = LL.lookup_func_exn "applyN" in
        let final_args = callee :: LL.const_int int_type real_args_count :: args in
        debug_log_action "finished(partial application)" "build_call" name;
        LL.build_call applyN final_args |> Result.ok)
end

let is_value_binding expr = (Typedtree.type_of_expr expr).typ_desc = Prim "int"

let rec codegen = 
  (* TODO:
     1) Add support for unit and bool type (n maybe)
     3) Add support for lambda as function argument
     4) Add support for if-expression 
     5) Add print *)
  function
  | Typedtree.TConst x -> 
      (match x with
      | PConst_int i -> LL.const_int int_type i |> Result.ok
      | PConst_bool b ->  LL.const_bool int_type b |> Result.ok)

  | Typedtree.TVar (name, _) ->
      (match Hashtbl.find_opt named_values name with 
        | None -> `NotBoundedVariable name |> Result.error
        | Some x -> x |> Result.ok)

  | Typedtree.TIf _ -> 
      `NotImplemented "if expr" |> Result.error

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

  | _ -> failwith "kek"

let codegen_top_level =
  function
  | Typedtree.TLet (_, "main", _, value_expr, _) ->
      debug_log_action "codegen_top_level(main)" "start" "main"; 
      main_body := Some value_expr;
      debug_log_action "codegen_top_level(main)" "finished" "main"; 
      () |> Result.ok

  | Typedtree.TLet (_, name, _, (Typedtree.TLam _ as lambda) , in_expr) ->
      debug_log_action "codegen_top_level(function)" "start" name; 
      let (args, body) = TypedtreeHelper.fold_lambda lambda in
      let _ = LlvmFunction.build_function name args body codegen in
      debug_log_action "codegen_top_level(function)" "finished" name; 
      codegen in_expr |> ignore |> Result.ok

  | Typedtree.TLet (_, name, _, value_expr, in_expr) ->
      debug_log_action "codegen_top_level(value)" "start" name;
      let global_type = LlvmFunction.to_llvm_type (Typedtree.type_of_expr value_expr) in 
      let global_value = Llvm.define_global name (Llvm.const_null global_type) the_module in
      Hashtbl.add global_initializers name (global_type, value_expr);
      Hashtbl.add named_values name global_value;
      debug_log_action "codegen_top_level(value)" "finished" name;
      codegen in_expr |> ignore |> Result.ok

  | _ -> failwith "incorrect toplevel"

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