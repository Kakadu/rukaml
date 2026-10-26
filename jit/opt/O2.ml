open Common

let name = "o2"
let requires = []
let init_targets = lazy (Llvm_all_backends.initialize ())

let run_on_module ?(passes = "default<O2>") m =
  Lazy.force init_targets;
  let triple = Llvm_target.Target.default_triple () in
  let target = Llvm_target.Target.by_triple triple in
  let machine =
    Llvm_target.TargetMachine.create
      ~triple
      ~level:Llvm_target.CodeGenOptLevel.Aggressive
      target
  in
  let dl =
    Llvm_target.DataLayout.as_string (Llvm_target.TargetMachine.data_layout machine)
  in
  Llvm.set_target_triple triple m;
  Llvm.set_data_layout dl m;
  let opts = Llvm_passbuilder.create_passbuilder_options () in
  Fun.protect
    ~finally:(fun () -> Llvm_passbuilder.dispose_passbuilder_options opts)
    (fun () ->
       match Llvm_passbuilder.run_passes m passes machine opts with
       | Ok () -> ()
       | Error msg -> failwith ("opt: PassBuilder failed: " ^ msg))
;;

let run (ir : IR.t) : IR.t =
  with_parsed_ir ir (fun _ctx m ->
    run_on_module m;
    true)