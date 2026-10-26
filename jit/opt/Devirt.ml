open Common

let name = "devirt"
let requires = []

let run_on_module ?(is_hot = fun _ -> true) ctx m : bool =
  let sites =
    fold_module_instrs
      (fun acc ins ->
         if not (is_named ins "rukaml_applyN")
         then acc
         else (
           match chain_site_of m ~require_const:false ~is_hot ins with
           | Some s -> s :: acc
           | None -> acc))
      []
      m
    |> List.rev
  in
  match sites with
  | [] -> false
  | _ ->
    let repl : Llvm.llvalue ValTbl.t = ValTbl.create 64 in
    let resolve a = Option.value (ValTbl.find_opt repl a) ~default:a in
    let rewrite s =
      let ret_ty = Option.value (ret_type_of ctx s.target) ~default:(Llvm.i64_type ctx) in
      let param_tys = Array.map Llvm.type_of (Llvm.params s.target) in
      let fty = Llvm.function_type ret_ty param_tys in
      let args = Array.map resolve s.args in
      let b = Llvm.builder ctx in
      Llvm.position_before s.ins b;
      let call = Llvm.build_call fty s.target args "" b in
      Llvm.replace_all_uses_with s.ins call;
      ValTbl.replace repl s.ins call;
      delete_site_chain s
    in
    List.iter rewrite sites;
    true
;;

let run (ir : IR.t) : IR.t = with_parsed_ir ir (fun ctx m -> run_on_module ctx m)
