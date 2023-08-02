open Compile_lib
open ANF2
open Miniml

[@@@ocaml.warnerror "-3"]

let failwiths fmt = Format.kasprintf failwith fmt

type config = { mutable verbose : bool }

let cfg = { verbose = false }

let log fmt =
  if cfg.verbose then Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt
  else Format.ifprintf Format.std_formatter fmt

let set_verbose b = cfg.verbose <- b
(* log "setting verbose output of LLVM_impl" *)

module type TOP_DEFS = sig
  val add : string -> Llvm.lltype -> unit
  val find_typ_exn : string -> Llvm.lltype
end

module String_map = Map.Make (String)

module Top_defs = struct
  let store : Llvm.lltype String_map.t ref = ref String_map.empty
  let add name typ = store := String_map.add name typ !store
  let find_typ_exn name = String_map.find name !store
end

let on_vb (module LL : LL.S) (module TD : TOP_DEFS) : ANF2.vb -> _ =
 fun (_flg, name, body) ->
  log "vb %s" name;

  let top_look_exn name =
    try (LL.lookup_func_exn name, TD.find_typ_exn name)
    with Not_found -> failwiths "Toplevel name %S not found" name
  in
  (* It looks like number of arguments is a number of abstractions.
     because we can return functions; for example `(fun _ -> fac)` *)
  let virt_of_named_hash : (_, Llvm.llvalue) Hashtbl.t = Hashtbl.create 23 in
  let virt_of_name s =
    match Hashtbl.find virt_of_named_hash s with
    | e -> e
    | exception Not_found ->
        failwiths "Can't find virtual register for name '%s'" s
  in
  let add_virt_binding ~key v = Hashtbl.add virt_of_named_hash key v in
  let remove_virt_binding key = Hashtbl.remove virt_of_named_hash key in
  let with_virt_binding ~key data ~f =
    add_virt_binding ~key data;
    let rez = f () in
    remove_virt_binding key;
    rez
  in
  let i64_typ = Llvm.i64_type LL.context in

  let rec gen_a = function
    | AConst (Miniml.Parsetree.PConst_int n) -> LL.const_int i64_typ n
    | AVar name when Hashtbl.mem virt_of_named_hash name -> virt_of_name name
    | AVar name ->
        assert (LL.has_toplevel_func name);
        let alloc_closure, typ = top_look_exn "rukaml_alloc_closure" in

        log "%s %d" __FILE__ __LINE__;
        log "typ of alloc_closure = %S" (Llvm.string_of_lltype typ);
        let final_args =
          let f = LL.lookup_func_exn name in
          let formal_params_count = LL.params f |> Array.length in
          let ptr = LL.build_pointercast f i64_typ in
          [ ptr; LL.const_int i64_typ formal_params_count ]
        in
        LL.build_call typ alloc_closure final_args
    | ATuple (a, b, []) ->
        let a = gen_a a in
        let b = gen_a b in
        let alloc, typ = top_look_exn "rukaml_alloc_pair" in
        LL.build_call typ alloc [ a; b ]
        (* assert false *)
    | anf ->
        Format.eprintf "ANF: %a\n%!" ANF2.pp_a anf;
        Format.eprintf "virt_of_named_hash.card = %d\n%!"
          (Hashtbl.length virt_of_named_hash);
        failwiths "Unsupported case %s %d" __FUNCTION__ __LINE__
  and gen_c anf =
    let is_fully_applied name args =
      assert (LL.has_toplevel_func name);
      let formal_params = LL.params (LL.lookup_func_exn name) in
      Array.length formal_params = List.length args
    in

    match anf with
    | CAtom a -> gen_a a
    | CApp (APrimitive "field", AConst (PConst_int n), [ what ]) ->
        let source = gen_a what in
        let accessor, accessor_typ = top_look_exn "rukaml_field" in
        LL.build_call accessor_typ accessor [ LL.const_int i64_typ n; source ]
    | CApp (AVar fname, arg1, args)
      when LL.has_toplevel_func fname && is_fully_applied fname (arg1 :: args)
      ->
        (* Full appication of toplevel function. We can omit primitives for partial application and make a direct call  *)
        let f, ftyp = top_look_exn fname in
        let final_args = List.map gen_a (arg1 :: args) in
        LL.build_call ftyp f final_args
        (* Format.eprintf "ANF: %a\n%!" ANF2.pp_c anf;
           failwiths "Unsupported case %s %d" __FUNCTION__ __LINE__ *)
    | CApp (AVar f, arg1, args) when LL.has_toplevel_func f ->
        (* log "%S is a toplevel func but not fully applied" f; *)
        let generated_args = List.map gen_a (arg1 :: args) in
        (* List.iteri
           (fun i v -> log "  arg %d: %s" i (Llvm.string_of_llvalue v))
           generated_args; *)
        let f = LL.lookup_func_exn f in

        let fptr =
          let alloc_closure, alloc_typ = top_look_exn "rukaml_alloc_closure" in
          let formal_argsc = LL.params f |> Array.length in
          LL.build_call alloc_typ alloc_closure
            [
              LL.build_pointercast f i64_typ; LL.const_int i64_typ formal_argsc;
            ]
        in
        let final_args =
          fptr
          :: LL.const_int i64_typ (List.length generated_args)
          :: generated_args
        in
        let applyN, typ = top_look_exn "rukaml_applyN" in

        (* log "%s %d. applyN  =  %s" __FUNCTION__ __LINE__
           (Llvm.string_of_llvalue applyN); *)
        log "%s %d" __FILE__ __LINE__;
        let rez = LL.build_call typ applyN final_args in
        (* let (_ : Llvm.llvalue) =
             LL.set_metadata rez "result_of_partial_application" ""
           in *)
        rez
    | CApp (AVar f, arg1, args) when Hashtbl.mem virt_of_named_hash f ->
        let generated_args = List.map gen_a (arg1 :: args) in
        let final_args =
          virt_of_name f
          :: LL.const_int i64_typ (List.length generated_args)
          :: generated_args
        in
        let applyN, typ = top_look_exn "rukaml_applyN" in
        let rez = LL.build_call typ applyN final_args in
        (* let (_ : Llvm.llvalue) =
             LL.set_metadata rez "result_of_application_of_localvar" ""
           in *)
        rez
    | CApp (APrimitive (("+" | "-" | "*" | "/") as prim), arg1, [ arg2 ]) -> (
        let arg1 = gen_a arg1 in
        let arg2 = gen_a arg2 in
        match prim with
        | "+" -> LL.build_add arg1 arg2
        | "*" -> LL.build_mul arg1 arg2
        | "-" -> LL.build_sub arg1 arg2
        | "/" -> LL.build_sdiv arg1 arg2
        | _ -> assert false)
    | CApp (APrimitive "=", arg1, [ arg2 ]) ->
        let arg1 = gen_a arg1 in
        let arg2 = gen_a arg2 in
        let rez = LL.build_icmp Llvm.Icmp.Eq arg1 arg2 in
        Llvm.build_zext rez i64_typ "name" LL.builder
    | CIte (cond, then_, else_) ->
        let cond =
          let cond_rez = gen_a cond in
          LL.build_icmp Llvm.Icmp.Ne ~name:"ifcond" cond_rez
            (Llvm.const_int (Llvm.i64_type LL.context) 0)
        in

        (* get current function since basic blocks have to be inserted into a function *)
        let func = Llvm.block_parent (Llvm.insertion_block LL.builder) in
        (* Following the LLVM tutorial: https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl05.html *)
        (* create conditional branch *)
        let then_bb, else_bb, merge_bb =
          let f x =
            Llvm.append_block LL.context (Llvm.value_name func ^ x) func
          in
          (f "_then", f "_else", f "_cont")
        in
        let (_ : Llvm.llvalue) =
          Llvm.build_cond_br cond then_bb else_bb LL.builder
        in
        (* then branch *)
        let t =
          Llvm.position_at_end then_bb LL.builder;
          gen then_
        in
        let (_ : Llvm.llvalue) = Llvm.build_br merge_bb LL.builder in
        let then_bb = Llvm.insertion_block LL.builder in
        (* else branch *)
        let e =
          Llvm.position_at_end else_bb LL.builder;
          gen else_
        in
        let else_bb = Llvm.insertion_block LL.builder in
        Llvm.build_br merge_bb LL.builder |> ignore;
        (* merge point *)
        Llvm.position_at_end merge_bb LL.builder;
        Llvm.build_phi [ (t, then_bb); (e, else_bb) ] "phi_result" LL.builder
    | anf ->
        Format.eprintf "ANF: %a\n%!" ANF2.pp_c anf;
        failwiths "Unsupported case %s %d" __FUNCTION__ __LINE__
  and gen : _ -> Llvm.llvalue = function
    | ELet (_, Parsetree.PTuple _, _, _) -> assert false
    | ELet (_, Miniml.Parsetree.PVar name, rhs, wher) ->
        let new_virt = gen_c rhs in
        with_virt_binding ~key:name new_virt ~f:(fun () ->
            let rez = gen wher in
            rez)
    | EComplex c -> gen_c c
  in

  let args, body = ANF2.group_abstractions body in
  let fun_typ =
    let args = Array.make (List.length args) i64_typ in
    Llvm.function_type i64_typ args
  in
  let the_function = Llvm.declare_function name fun_typ LL.module_ in
  List.iteri
    (fun n (ANF2.APname key) ->
      let param = Llvm.param the_function n in
      log "  formal parameter %d: %s" n (Llvm.string_of_llvalue param);
      add_virt_binding ~key param)
    args;
  let bb = Llvm.append_block LL.context "entry" the_function in
  Llvm.position_at_end bb LL.builder;

  let __ _ =
    let f = LL.lookup_func_exn "myputc" in
    LL.build_call (TD.find_typ_exn "myputc") f [ Llvm.const_int i64_typ 0x30 ]
  in
  let return_val = gen body in
  let (_ : Llvm.llvalue) = Llvm.build_ret return_val LL.builder in

  (* log "@[%a@]\n===\n" LL.pp_value the_function; *)

  (* Llvm.dump_value the_function; *)

  (* Validate the generated code, checking for consistency. *)
  (match Llvm_analysis.verify_function the_function with
  | true -> TD.add name fun_typ
  | false ->
      Stdlib.Format.printf "invalid function generated\n%s\n"
        (Llvm.string_of_llvalue the_function);
      Llvm_analysis.assert_valid_function the_function);
  (* Optimize the function. TODO *)
  (* let (_ : bool) = Llvm.PassManager.run_function the_function the_fpm in *)
  (* Llvm.dump_value the_function; *)
  ()

let codegen : ANF2.vb list -> _ =
 fun anf out_file ->
  Format.printf "%a\n%!" Compile_lib.ANF2.pp_stru anf;

  let context = Llvm.global_context () in
  (* Llvm.set_opaque_pointers context false; *)
  let builder = Llvm.builder context in
  let () = assert (Llvm_executionengine.initialize ()) in
  let the_module = Llvm.create_module context "main" in
  (* let the_execution_engine = Llvm_executionengine.create the_module in *)
  (* let the_fpm = Llvm.PassManager.create_function the_module in *)
  let module LL = (val LL.make context builder the_module) in
  let lama_int_type = Llvm.i64_type LL.context in

  (* let lama_ptr_type = Llvm.pointer_type2 context in *)

  (* let prepare_main () =
       let ft =
         (* TODO main has special args *)
         let args = Array.make 0 lama_ptr_type in
         (* Llvm.function_type lama_ptr_type args *)
         Llvm.function_type lama_int_type args
       in
       let the_function = Llvm.declare_function "main" ft the_module in
       (* Set names for all arguments. *)
       (* Base.Array.iteri (Llvm.params the_function) ~f:(fun i a ->
           let name = List.nth args i in
           Llvm.set_value_name name a;
           Base.Hashtbl.add_exn named_values ~key:name ~data:a); *)
       (* Create a new basic block to start insertion into. *)
       let bb = Llvm.append_block context "entry" the_function in
       Llvm.position_at_end bb builder;
       (* Add all arguments to the symbol table and create their allocas. *)
       let return_val =
         let c = Llvm.const_int lama_int_type 0x30 in
         let _ =
           let f = LL.lookup_func_exn "myputc" in
           LL.build_call (Llvm.type_of f) f [ c ]
         in
         Llvm.const_int lama_int_type 0
       in
       let (_ : Llvm.llvalue) = Llvm.build_ret return_val builder in
       (* Validate the generated code, checking for consistency. *)
       (match Llvm_analysis.verify_function the_function with
       | true -> ()
       | false ->
           Stdlib.Format.printf "invalid function generated\n%s\n"
             (Llvm.string_of_llvalue the_function);
           Llvm_analysis.assert_valid_function the_function);
       (* Optimize the function. *)
       let (_ : bool) = Llvm.PassManager.run_function the_function the_fpm in
       ()
     in *)
  let _ =
    let name = "myputc" in
    let typ = Llvm.function_type (Llvm.void_type context) [| lama_int_type |] in
    Top_defs.add name typ;
    Llvm.declare_function name typ the_module
  in
  let _ =
    (* void* lama_applyN(void* f, int32_t, ...) *)
    let name = "rukaml_applyN" in
    let typ =
      Llvm.var_arg_function_type lama_int_type
        [| lama_int_type; lama_int_type |]
    in
    Top_defs.add name typ;
    Llvm.declare_function name typ the_module
  in
  let _ =
    let name = "rukaml_alloc_closure" in
    let typ =
      Llvm.function_type lama_int_type [| lama_int_type; lama_int_type |]
    in
    Top_defs.add name typ;
    Llvm.declare_function name typ the_module
  in
  let _ =
    let name = "rukaml_alloc_pair" in
    let typ =
      Llvm.function_type lama_int_type [| lama_int_type; lama_int_type |]
    in
    Top_defs.add name typ;
    Llvm.declare_function name typ the_module
  in

  let _ =
    let name = "rukaml_field" in
    let typ =
      (* TODO: Should first argument be untagged?  *)
      Llvm.function_type lama_int_type [| lama_int_type; lama_int_type |]
    in
    Top_defs.add name typ;
    Llvm.declare_function name typ the_module
  in
  (* prepare_main (); *)
  List.iter (on_vb (module LL) (module Top_defs : TOP_DEFS)) anf;

  Llvm.print_module out_file the_module;
  Llvm.dispose_module the_module;

  Result.ok ()
