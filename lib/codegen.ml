open Machine

[@@@ocaml.warnerror "-8-21"]

let locals_pos : (string, int) Hashtbl.t = Hashtbl.create 53
let local_offset_exn l = ROffset (SP, 8 * Hashtbl.find locals_pos l)

let next_label =
  let last = ref 0 in
  fun () ->
    incr last;
    Printf.sprintf "lab%d" !last

let codegen_exp rdest : AST.expr -> _ = function
  | AST.EBinop ("*", EVar l, EVar r) ->
      emit sd t3 (local_offset_exn l);
      emit sd t4 (local_offset_exn r);
      emit mulw rdest t3 t4
  | EBinop ("-", EVar l, EConst r) ->
      emit sd t3 (local_offset_exn l);
      emit li t4 r;
      emit sub rdest t3 t4
  | expr ->
      emit comment (Format.asprintf "%a" AST.pp_expr expr);
      emit comment (Format.asprintf "Not implemented %d" __LINE__)

let rec codegen_stmt : AST.stmt -> _ = function
  | AST.Assgn (l, EConst c) when Hashtbl.mem locals_pos l ->
      emit li t0 c;
      emit sd t0 (local_offset_exn l)
  | AST.Assgn (l, rhs) when Hashtbl.mem locals_pos l ->
      (* codegen_exp rhs; *)
      codegen_exp t0 rhs;
      emit sd t0 (local_offset_exn l)
      (* emit comment (Format.asprintf "Not implemented %d" __LINE__) *)
  | AST.While (econd, body) ->
      let lab_begin = next_label () in
      let lab_fin = next_label () in
      emit label lab_begin;
      (match econd with
      | EBinop (">", EVar v, EConst 0) ->
          emit ld t0 (local_offset_exn v);
          emit bge zero t0 lab_fin
      | econd ->
          codegen_exp t0 econd;
          emit beq t0 zero lab_fin);
      List.iter codegen_stmt body;
      emit j lab_begin;
      emit label lab_fin
  | stmt ->
      emit comment (Format.asprintf "%a" AST.pp_stmt stmt);
      emit comment (Format.asprintf "Not implemented %d" __LINE__)

let codegen : AST.program -> Format.formatter -> unit =
 fun (locals, prog) ppf ->
  let printfn fmt =
    Format.kfprintf (fun ppf -> Format.fprintf ppf "\n") ppf fmt
  in

  List.iteri (fun i s -> Hashtbl.add locals_pos s i) locals;
  emit addi SP SP (-8 * List.length locals);
  List.iter codegen_stmt prog;

  (* Code to print everything *)
  ListLabels.iter locals ~f:(fun s ->
      emit comment (Printf.sprintf "trace variable %S" s);
      (* emit_write "varname_x" 2; *)
      emit la a0 (Printf.sprintf "varname_%s" s);
      emit li a1 (String.length s);
      emit ld a2 (local_offset_exn s);
      emit call "trace_variable";

      (* emit la a0 "helloworld"; *)
      (* emit_write "varname_x" 1; *)
      ());

  Machine.flush_queue ppf

let%expect_test _ =
  let s = {| x=5;
  acc=1;
  while x>0 do acc=acc*x; x=x-1;  done |} in
  Parser.init s;
  Hashtbl.add locals_pos "x" 0;
  Hashtbl.add locals_pos "acc" 1;

  (match Parser.program () with
  | None -> print_endline "can't parse"
  | Some p ->
      (* let open Format in *)
      (* printf "_start:\n"; *)
      codegen ([], p) Format.std_formatter;

      (* printf "  li      a0, 0 \n";
         printf "  li      a7, 93\n";
         printf "  ecall\n"; *)
      Format.print_flush ());
  [%expect {| |}]
