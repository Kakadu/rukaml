type cmd_args = { mutable file : string }

let args = { file = "" }
let fprintf = Format.fprintf
let printfn ppf fmt = Format.kfprintf (fun ppf -> fprintf ppf "\n") ppf fmt

let () =
  Arg.parse [] (fun s -> args.file <- s) "help";
  Parser.init @@ In_channel.with_open_text args.file In_channel.input_all;
  match Parser.program () with
  | None -> exit 1
  | Some p ->
      Out_channel.with_open_text "out.s" (fun outch ->
          let ppf = Format.formatter_of_out_channel outch in
          let locals = AST.all_vars p in

          printfn ppf ".global _start\n";
          printfn ppf "_start:\n";
          (* printfn ppf "  addi sp, sp, %+d*8" (AST.String_set.cardinal locals); *)
          Codegen.codegen (List.of_seq (AST.String_set.to_seq locals), p) ppf;
          printfn ppf "  li a0, 22";
          printfn ppf "  li a7, 93";
          printfn ppf "  ecall\n";

          Format.pp_print_flush ppf ())
