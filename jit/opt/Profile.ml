open Common

let name = "profile"
let requires = [ "rukaml_prof_tick"; "rukaml_prof_dump" ]

let run (cfg : config) (ir : IR.t) : IR.t =
  with_parsed_ir ir (fun ctx m ->
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
    true)
;;