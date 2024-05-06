type cmd_args = { mutable file : string }

let args = { file = "" }
let fprintf = Format.fprintf
(* let printfn ppf fmt = Format.kfprintf (fun ppf -> fprintf ppf "\n") ppf fmt *)

let () =
  Arg.parse [] (fun s -> args.file <- s) "help";
  Parser.init @@ In_channel.with_open_text args.file In_channel.input_all;
  match Parser.program () with
  | None -> exit 1
  | Some p ->
      Out_channel.with_open_text "out.s" (fun outch ->
          let ppf = Format.formatter_of_out_channel outch in
          let locals = AST.all_vars p in

          let printfn fmt =
            Format.kfprintf (fun ppf -> fprintf ppf "\n") ppf fmt
          in

          printfn ".section .data";
          printfn "helloworld: .string \"Hello_World!\\n\\0\"";
          printfn ".equ BUFSIZE,32";
          (* printfn
             "the_buffer: .string \"01234567012345670123456701234567\\n\" # \
              32+1 bytes"; *)
          locals
          |> AST.String_set.iter (fun s ->
                 printfn "varname_%s: .string  \"%s\\n\"" s s;
                 printfn ".equ VARLEN_%s, %d+1" s (String.length s));

          printfn ".text";
          printfn ".extern memset";
          printfn ".extern trace_variable";
          printfn ".global _start";
          printfn "_start:";
          printfn "  .option push";
          printfn "  .option norelax";
          printfn "  la gp, __global_pointer$";
          printfn "  .option pop";
          (* printfn ppf "  addi sp, sp, %+d*8" (AST.String_set.cardinal locals); *)
          Codegen.codegen (List.of_seq (AST.String_set.to_seq locals), p) ppf;

          printfn "  li a0, 22";
          printfn "  li a7, 93";
          printfn "  ecall\n";

          Format.pp_print_flush ppf ())
