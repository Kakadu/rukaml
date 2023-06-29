let codegen anf out_file =
  Format.printf "%a\n%!" Compile_lib.ANF2.pp_stru anf;

  let context = Llvm.global_context () in
  let builder = Llvm.builder context in
  let () = assert (Llvm_executionengine.initialize ()) in
  let the_module = Llvm.create_module context "main" in
  let the_execution_engine = Llvm_executionengine.create the_module in
  let the_fpm = Llvm.PassManager.create_function the_module in
  let module LL = (val LL.make builder the_module) in
  let i64_type = Llvm.i64_type context in
  let i32_type = Llvm.i32_type context in
  let lama_int_type = i32_type in
  let lama_ptr_type = Llvm.pointer_type lama_int_type in

  let prepare_main () =
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
    (* Finish off the function. *)
    let return_val =
      (* let temp = Llvm.build_alloca lama_ptr_type "a" builder in
         let body = codegen_expr body in
         (* let body = Llvm.build_inttoptr body lama_ptr_type "aa" builder in *)
         let _ = Llvm.build_store body temp builder in
         let tt = Llvm.build_load temp "b" builder in *)
      let c = Llvm.const_int lama_int_type 0x30 in
      (* let __ () =
           Llvm.(
             build_call
               (lookup_function "printf" the_module |> Option.get)
               [| const_stringz context "%d"; c |])
             "" builder
         in *)
      let _ = LL.build_call LL.(lookup_func_exn "myputc") [ c ] in

      Llvm.const_int lama_int_type 0
    in
    let (_ : Llvm.llvalue) = Llvm.build_ret return_val builder in
    (* Validate the generated code, checking for consistency. *)
    (* Llvm.dump_module the_module; *)
    (* log "%s %d" __FILE__ __LINE__; *)
    (match Llvm_analysis.verify_function the_function with
    | true -> ()
    | false ->
        Stdlib.Format.printf "invalid function generated\n%s\n"
          (Llvm.string_of_llvalue the_function);
        Llvm_analysis.assert_valid_function the_function);
    (* Optimize the function. *)
    let (_ : bool) = Llvm.PassManager.run_function the_function the_fpm in
    (* Llvm.dump_value the_function; *)
    ()
  in
  let _ =
    Llvm.declare_function "myputc"
      (Llvm.function_type (Llvm.void_type context) [| lama_int_type |])
      the_module
  in
  prepare_main ();

  Llvm.print_module out_file the_module;

  Result.ok ()
