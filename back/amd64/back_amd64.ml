(* let is_log_enabled = ref false

   let set_logging b =
     is_log_enabled := b;
     Amd64_impl.set_verbose b

   let log fmt =
     if !is_log_enabled then Format.kasprintf (Format.printf "%s\n%!") fmt
     else Format.ifprintf Format.std_formatter fmt

   let extra_switches =
     [
       ("-amd64-v", Arg.Unit (fun () -> set_logging true), " verbose Amd64 backend");
     ]

   type error = [ Miniml.Inferencer.error | `Not_implemented ]

   let pp_error ppf _ = Format.fprintf ppf "error?"

   let compile parsed _typed ~filename =
     let ( let* ) = Result.bind in
     let lifted_stru = CConv.(structure ~standart_globals) parsed in
     let* typedtree = Miniml.Inferencer.structure lifted_stru in
     let anf = Compile_lib.ANF2.(anf_stru typedtree |> simplify_stru) in
     Amd64_impl.codegen anf filename

   let () =
     let module B = struct
       type nonrec error = error

       let pp_error = pp_error
       let run = compile
     end in
     Registration.register_backend_exn "amd64" (module B) extra_switches *)
