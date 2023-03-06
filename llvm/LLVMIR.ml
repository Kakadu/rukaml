
module Format = Stdlib.Format
open Llvm
open Miniml

let context = Llvm.global_context ()
let builder = Llvm.builder context
let int_type = Llvm.i64_type context
let bool_type = Llvm.i1_type context
let unit_type = Llvm.void_type context


let the_module = Llvm.create_module context "main"
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let functions_with_lambda_args:(string, ( (string * Typedtree.ty) list * Typedtree.expr)) Hashtbl.t = Hashtbl.create 10
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
  ]

let pp_error ppf: error -> _  = function
  | `NotBoundedVariable x -> Format.fprintf ppf "Variable not bounded: %s" x
  | `NotImplemented x -> Format.fprintf ppf "Not implemented: %s" x
  | `LambdaOutsideLet -> Format.fprintf ppf "Lambda outside let"
  | `UnexpectedReturnType -> Format.fprintf ppf "Ubexpected return type"
  | `IncorrectFunction -> Format.fprintf ppf "Incorrect function"
  | `NotFunctionApplication -> Format.fprintf ppf "Not function application"

let list_or_error lst =
  let err = List.find_opt (fun x -> Result.is_error x) lst in
  match err with
  | None -> let lst = List.map Result.get_ok lst in Result.ok lst
  | Some (Result.Error e) -> Result.error e
  | _ -> failwith "__unreachable__"

let (<<) f g x = f(g(x))
let ( let* ) x f = Result.bind x f

module TypedtreeHelper = struct 
  let fold_lambda x = 
    let get_arg_type =
      function
      | Typedtree.Arrow (x, _) -> x
      | _ -> failwith ""
    in
    let rec helper lambda args =
      match lambda with
      | Typedtree.TLam (name, e, t) ->
        let t = get_arg_type t.typ_desc in
        helper e ((name, t) :: args) 
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
end

module LlvmFunction = struct
  let is_prim_arg (t: Typedtree.ty) =
    (* Typedtree.pp_ty Format.std_formatter t; *)
    match t.typ_desc with
    | Typedtree.Prim "int" | Typedtree.Prim "bool" | Typedtree.Prim "unit" -> true
    | _ -> false

  let to_llvm_prim_arg (t: Typedtree.ty) =
    match t.typ_desc with
    | Typedtree.Prim "int" ->  int_type 
    | Typedtree.Prim "bool" -> bool_type
    | Typedtree.Prim "unit" -> unit_type
    | _ -> failwith ""

  let has_lambda_arg = List.exists (not << is_prim_arg)

  let get_substitution args_sign values =
    assert (List.length args_sign = List.length values);
    List.combine args_sign values |> List.filter (not << is_prim_arg << snd << fst)

  (* let get_prim_args = List.filter (is_prim_arg << snd)
  let get_lambda_args = List.filter (not << is_prim_arg << snd) *)
  let get_llvm_args = (List.map (to_llvm_prim_arg << snd)) << (List.filter (is_prim_arg << snd))

  let build_function name args body codegen =
    let args_names = List.map fst args in
    let args_types = get_llvm_args args |> Array.of_list in 
    (* 
    Format.printf "name: %s\n" name;
    Format.printf "x: %d\n" (List.length args_names);
    Format.printf "y: %d\n" (Array.length args_types); *)

    let function_type = Llvm.function_type int_type args_types in
    let f = LL.declare_function name function_type in
    let basic_block = Llvm.append_block context "entry" f in
    LL.position_at_end basic_block;
    
    Hashtbl.add named_values name f;
    Array.iter (fun (arg, arg_name) ->
      set_value_name arg_name arg;
      Hashtbl.add named_values arg_name arg;
    ) (Array.combine (params f) (Array.of_list args_names));

    let ret_val = codegen body in
    Result.bind ret_val (Result.ok << LL.build_ret) |> ignore

  let build_main body codegen = 
    let function_type = Llvm.function_type int_type [| unit_type |] in
    let f = LL.declare_function "real_main" function_type in
    let basic_block = Llvm.append_block context "entry" f in
    LL.position_at_end basic_block;
    let ret_val = codegen body in
    Hashtbl.add named_values "main" f;
    Result.bind ret_val (Result.ok << LL.build_ret) |> ignore

  let build_call name args codegen = 
    if has_lambda_arg (List.map Typedtree.type_of_expr args) then `NotImplemented "lambda args" |> Result.error
    else
      let* args = List.map codegen args |> list_or_error in
      if Builtins.is_builtin_binop name then
        match args with
        | [arg1; arg2] -> Builtins.call_builtin_binop name arg1 arg2 |> Result.ok
        | _ ->  `NotImplemented "" |> Result.error
      else
        let calling_f = LL.lookup_func_exn name in
        LL.build_call calling_f args |> Result.ok
end


let rec codegen = 
  (* TODO:
     1) Add support for unit and bool type (n maybe)
     2) Add support for partial application
     3) Add support for lambda as function argument
     4) Add support for if-expression 
     5) Add print *)
  function
  | Typedtree.TConst x -> 
      LL.const_int int_type x |> Result.ok

  | Typedtree.TVar (name, _) ->
      (match Hashtbl.find_opt named_values name with 
        | None -> `NotBoundedVariable name |> Result.error
        | Some x -> x |> Result.ok)

  | Typedtree.TIf _ -> 
      `NotImplemented "if expr" |> Result.error 

  | Typedtree.TLam _ ->
      Result.error `LambdaOutsideLet

  | Typedtree.TLet (_, "main", _, value_expr, in_expr) -> 
      LlvmFunction.build_main value_expr codegen;
      codegen in_expr
  
  | Typedtree.TLet (_, name, _, (Typedtree.TLam _ as lambda) , in_expr) ->
      let (args, body) = TypedtreeHelper.fold_lambda lambda in
      LlvmFunction.build_function name args body codegen;
      codegen in_expr

  | Typedtree.TLet (_, pattern, _, value_expr, in_expr) -> 
      let* value = codegen value_expr in
      Hashtbl.add named_values pattern value;
      codegen in_expr

  | Typedtree.TApp _ as app ->
      let (args, f) = TypedtreeHelper.fold_application app in
      (match f with
      | Typedtree.TVar (name, _) -> LlvmFunction.build_call name args codegen
      | _ -> Result.error `NotFunctionApplication)

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
  |>  Typedtree.compact_expr 

let generate_llvm (value_bindings: Typedtree.value_binding list) emit_llvm llvm_out object_out = 
  Builtins.declare_builtins ();
  let result = List.map (codegen << value_binding_to_expr) value_bindings |> list_or_error in 
  match result with
  | Result.Ok _ -> dump emit_llvm object_out llvm_out |> Result.ok
  | Result.Error e -> e |> Result.error