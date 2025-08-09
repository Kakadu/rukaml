(* https://flint.cs.yale.edu/cs421/papers/x86-asm/asm.html
   https://jvns.ca/blog/2021/05/17/how-to-look-at-the-stack-in-gdb
   https://en.wikipedia.org/wiki/X86_calling_conventions
   https://github.com/jhucompilers/fall2022/tree/gh-pages/lectures
*)

(* We are using Intel syntax !! *)

(* NOTE: Possible fuckups

   1) Exit code is not any possible int. See https://tldp.org/LDP/abs/html/exitcodes.html
   2) We have runtime that could call back rukaml function after full partiall application.
   This means that SYSV calling convention clashes with stdcall-ish. So, in runtime we call
   the rukaml function with 6 zero arguments and real ones, to make real ones to go to the
   stack explicitly
   3) It's easy to forget that after function prologue argments start from RSP+2*8
   (RBP and code ptr take two words)
   4) arguments go to the stack from rigth to the left (unexpected order)
   5) Variadic functions should AL:=0 to say that we don't have floating arguments.
*)

let failwiths fmt = Format.kasprintf failwith fmt

type config = { mutable verbose : bool }

let cfg = { verbose = false }

let log fmt =
  if cfg.verbose
  then Format.kasprintf (Format.printf "%s\n%!") fmt
  else Format.ifprintf Format.std_formatter fmt
;;

let set_verbose b =
  (* Printf.printf "verbosity in %s = %b\n%!" __FILE__ b; *)
  cfg.verbose <- b
;;

let fprintf = Format.fprintf
let printfn ppf fmt = Format.kfprintf (fun ppf -> fprintf ppf "\n") ppf fmt

let pp_space_list eta =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") eta
;;

let print_prologue ppf name =
  if name = "main"
  then (
    printfn ppf "global _start";
    printfn ppf "_start:")
  else printfn ppf "_%s:" name;
  (* printfn ppf "%s:" name; *)
  printfn ppf "  push rbp";
  printfn ppf "  mov  rbp, rsp";
  (* movq dst, src *)
  (* printfn ppf "  ;sub rsp, 24 ; given 24 is total size of local variables"; *)
  fprintf ppf "%!"
;;

let print_epilogue ppf name =
  printfn ppf "  pop rbp";
  printfn ppf "  ret  ;;;; %s" name;
  fprintf ppf "%!"
;;

module ANF = Compile_lib.ANF
module Ident = Frontend.Ident

let gensym =
  let open ANF in
  reset_gensym ();
  gensym
;;

let gen_name ?(prefix = "") () = Printf.sprintf "%s%d" prefix (gensym ())

let list_take_n n xs =
  let rec helper n forw xs =
    if n = 0
    then List.rev forw, xs
    else (
      match xs with
      | [] -> failwith "bad argument"
      | h :: tl -> helper (n - 1) (h :: forw) tl)
  in
  helper n [] xs
;;

let list_take n xs =
  (* TODO: it's not optimal *)
  fst (list_take_n n xs)
;;

type dest =
  | DReg of string
  | DStack_var of Frontend.Ident.t

module Addr_of_local = struct
  let store : (Frontend.Ident.t, _) Hashtbl.t = Hashtbl.create 13
  let last_pos = ref 0
  let get_locals_count () = !last_pos

  let clear () =
    Hashtbl.clear store;
    last_pos := 0
  ;;

  let extend name =
    incr last_pos;
    (* log "extend %s with shift = %d" name !last_pos; *)
    Hashtbl.add store name !last_pos
  ;;

  let remove_local name =
    let pos = Hashtbl.find store name in
    if !last_pos = pos
    then (
      (* log "remove %s with shift = %d" name !last_pos; *)
      decr last_pos;
      Hashtbl.remove store name)
    else
      failwiths "Something bad %d. Can't remove local variable %a" __LINE__ Ident.pp name
  ;;

  let count () = Hashtbl.length store
  let size = count
  let contains name = Hashtbl.mem store name
  let has_key = contains

  let find_exn name =
    match Hashtbl.find store name with
    | v -> v
    | exception Not_found ->
      failwiths "Can't find location of a variable %a" Ident.pp name
  ;;

  let lookup_exn = find_exn

  let add_arg ~argc i name =
    assert (i < argc);
    let loc = -2 - argc + 1 + i in
    assert (loc < 0);
    log "Location argument \"%a\" in [rbp+%d]" Ident.pp name (-loc);
    Hashtbl.add store name loc
  ;;

  let remove_args xs =
    log "Removing info about args [ %a ]" (pp_space_list Ident.pp) xs;
    List.iter (Hashtbl.remove store) xs
  ;;

  let pp_local_exn ppf name =
    let offset = find_exn name in
    (* 8 for 64 bit, 4 for 32bit *)
    if offset > 0
    then fprintf ppf "[rbp-%d*8]" offset
    else fprintf ppf "[rbp+%d*8]" (-offset)
  ;;

  let keys () =
    Hashtbl.to_seq_keys store
    |> Seq.fold_left (fun acc x -> Format.asprintf "%s %a" acc Ident.pp x) ""
  ;;
end

let pp_dest ppf = function
  | DReg s -> fprintf ppf "%s" s
  | DStack_var name -> Addr_of_local.pp_local_exn ppf name
;;

let alloc_closure ppf name arity =
  printfn ppf "  mov rdi, %a" Ident.pp name;
  printfn ppf "  mov rsi, %d" arity;
  printfn ppf "  call rukaml_alloc_closure"
;;

let allocate_locals ppf input_anf : now:unit -> unit =
  let names = ref [] in
  let rec helper = function
    | ANF.EComplex c -> helper_c c
    | ELet (_flg, Tpat_var name, rhs, where_) ->
      Addr_of_local.extend name;
      names := name :: !names;
      helper_c rhs;
      helper where_
    | ELet (_, Tpat_tuple (_, _, _), _, _) -> assert false
  and helper_c = function
    | CIte (_, th, el) ->
      helper th;
      helper el
    | CApp _ | CAtom _ -> ()
  in
  helper input_anf;
  let count = List.length !names in
  assert (count = Addr_of_local.get_locals_count ());
  (* If assertion fails it's like a number of locals with the same names *)
  let args_repr = Ident.concat_str !names in
  if count > 0
  then printfn ppf "  sub rsp, 8*%d ; allocate for local variables %s" count args_repr;
  let deallocate_padding =
    if count mod 2 = 1
    then (
      let pad_name = Ident.of_string @@ Printf.sprintf "__pad%d" (gensym ()) in
      printfn ppf "  sub rsp, 8 ; allocate padding for locals";
      Addr_of_local.extend pad_name;
      fun () ->
        Addr_of_local.remove_local pad_name;
        printfn ppf "  add rsp, 8 ; deallocate padding for locals")
    else fun () -> ()
  in
  if count > 0
  then (
    fun ~now ->
      let () = now in
      deallocate_padding ();
      printfn ppf "  add rsp, 8*%d ; deallocate local variables %s" count args_repr;
      !names |> List.iter Addr_of_local.remove_local)
  else
    fun ~now ->
      let () = now in
      ()
;;

let emit_alloc_closure ppf fname arity =
  printfn ppf "  mov rdi, %a" Ident.pp fname;
  printfn ppf "  mov rsi, %d" arity;
  printfn ppf "  call rukaml_alloc_closure"
;;

let list_iter_revindex ~f xs =
  let l = List.length xs in
  List.iteri (fun n x -> f (l - n - 1) x) xs
;;

(**
    Argument [is_toplevel] returns None or Some arity. *)
let generate_body is_toplevel ppf body =
  let open Frontend.Parsetree in
  let allocate_args args =
    (* log "XXX %s: [ %a ]" __FUNCTION__
       (Format.pp_print_list
          ~pp_sep:(fun ppf () -> fprintf ppf ", ")
          Compile_lib.ANF.pp_a)
       args; *)
    let count = List.length args in
    let _stack_padding =
      if count mod 2 = 0
      then 0
      else (
        printfn ppf "  sub rsp, 8 ; trying to save alignment 16 bytes";
        1)
    in
    printfn ppf "  sub rsp, 8*%d ; fun arguments" count;
    ListLabels.iteri args ~f:(fun i ->
      let pp_access ?(doc = "") v =
        Format.fprintf ppf "  mov qword [rsp%+d*8], %d" i v;
        if doc <> "" then printfn ppf " ; %s" doc else printfn ppf ""
      in
      function
      | Compile_lib.ANF.AUnit | AConst (PConst_bool false) -> pp_access 0
      | AConst (PConst_bool true) -> pp_access 1
      | AConst (PConst_int n) -> pp_access ~doc:"constant" n
      | AVar vname when Option.is_some (is_toplevel vname) ->
        (match is_toplevel vname with
         | Some arity ->
           emit_alloc_closure ppf vname arity;
           printfn ppf "  mov qword [rsp%+d*8], rax ; arg \"%a\"" i Ident.pp vname
         | None -> assert false)
      | AVar { Ident.hum_name = "print"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_print_int") 1;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar vname ->
        printfn
          ppf
          "  mov qword r8, %a  ; arg \"%a\""
          Addr_of_local.pp_local_exn
          vname
          Ident.pp
          vname;
        printfn ppf "  mov qword [rsp%+d*8], r8" (count - 1 - i)
      | ALam _ -> failwith "Should it be representable in ANF?"
      | APrimitive _ -> assert false
      | ATuple _ -> assert false);
    count + _stack_padding
  in
  let rec helper dest = function
    | Compile_lib.ANF.EComplex c -> helper_c dest c
    | ELet (_, Tpat_var name, rhs, wher) ->
      assert (Addr_of_local.contains name);
      let local = DStack_var name in
      (* printfn ppf "    ;; calculate rhs and put into %a. offset = %d" pp_dest
           dest
           (Addr_of_local.find_exn name); *)
      helper_c local rhs;
      helper dest wher
    | ELet (_, Tpat_tuple (_, _, _), _, _) -> assert false
  and helper_c (dest : dest) = function
    | CIte (CAtom (AConst (Frontend.Parsetree.PConst_bool true)), bth, _bel) ->
      helper dest bth
    | CIte (CAtom (AConst (Frontend.Parsetree.PConst_bool false)), _bth, bel) ->
      helper dest bel
    | CIte (CAtom (AVar econd), bth, bel) when Addr_of_local.contains econd ->
      (* if on global or local variable  *)
      printfn ppf "  mov qword rdx, %a" Addr_of_local.pp_local_exn econd;
      printfn ppf "  cmp rdx, 0";
      let el_lab = Printf.sprintf "lab_then_%d" (gensym ()) in
      let fin_lab = Printf.sprintf "lab_endif_%d" (gensym ()) in
      printfn ppf "  je %s" el_lab;
      helper dest bth;
      printfn ppf "  jmp %s" fin_lab;
      printfn ppf "%s:" el_lab;
      helper dest bel;
      printfn ppf "%s:" fin_lab
    | CApp (AVar f, arg1, [])
      when f.Ident.hum_name = "print"
           && is_toplevel f = None
           && not (Addr_of_local.has_key f) ->
      (match arg1 with
       | AVar v when Addr_of_local.has_key v ->
         let name1 = Ident.of_string @@ gen_name ~prefix:"pad" () in
         let name2 = Ident.of_string @@ gen_name ~prefix:"print_arg" () in
         Addr_of_local.extend name1;
         Addr_of_local.extend name2;
         printfn ppf "  add rsp, -8*2";
         printfn ppf "  mov r11, %a" Addr_of_local.pp_local_exn v;
         printfn ppf "  mov qword [rsp], r11";
         printfn ppf "  call rukaml_print_int ; short";
         printfn ppf "  add rsp, 8*2";
         Addr_of_local.remove_local name2;
         Addr_of_local.remove_local name1;
         printfn ppf "  mov %a, rax" pp_dest dest
       | AConst (PConst_int n) ->
         let name1 = Ident.of_string @@ gen_name ~prefix:"pad" () in
         let name2 = Ident.of_string @@ gen_name ~prefix:"print_arg" () in
         Addr_of_local.extend name1;
         Addr_of_local.extend name2;
         printfn ppf "  add rsp, -8*2";
         printfn ppf "  mov qword %a, %d" Addr_of_local.pp_local_exn name2 n;
         printfn ppf "  call rukaml_print_int";
         printfn ppf "  add rsp, 8*2";
         Addr_of_local.remove_local name2;
         Addr_of_local.remove_local name1;
         printfn ppf "  mov %a, rax" pp_dest dest
       | AConst (PConst_bool _)
       | AVar _ | APrimitive _
       | ATuple (_, _, _)
       | ALam (_, _)
       | AUnit -> failwith "Should not happen")
    | CApp (APrimitive "=", AConst (PConst_int l), [ AConst (PConst_int r) ]) ->
      (* TODO: user Addr_of_local.pp_local_exn *)
      if l = r
      then printfn ppf "  mov qword %a, 1" pp_dest dest
      else printfn ppf "  mov qword %a, 0" pp_dest dest
    | CApp (APrimitive "=", AConst (PConst_int n), [ AVar vname ])
    | CApp (APrimitive "=", AVar vname, [ AConst (PConst_int n) ]) ->
      printfn ppf "  mov qword r11, %a" Addr_of_local.pp_local_exn vname;
      printfn ppf "  mov qword r12, %d" n;
      printfn ppf "  cmp r11, r12";
      let eq_lab = Printf.sprintf "lab_%d" (gensym ()) in
      let exit_lab = Printf.sprintf "lab_%d" (gensym ()) in
      printfn ppf "  je %s" eq_lab;
      (* TODO: user Addr_of_local.pp_local_exn *)
      printfn ppf "  mov qword %a, 0" pp_dest dest;
      printfn ppf "  jmp %s" exit_lab;
      printfn ppf "%s:" eq_lab;
      printfn ppf "  mov qword %a, 1" pp_dest dest;
      printfn ppf "  jmp %s" exit_lab;
      printfn ppf "%s:" exit_lab
      (* failwiths "not implemented %d" __LINE__ *)
      (* let left_name = LoI.alloc_temp () in
           printfn ppf "  sub rsp, 8 ; allocate for var %S" left_name;
           let left_dest = DStack_var left_name in
           helper_a left_dest arg1;
           let right_name = LoI.alloc_temp () in
           printfn ppf "  sub rsp, 8 ; allocate for var %S" right_name;
           let right_dest = DStack_var right_name in
           helper_a right_dest arg2;
           printfn ppf "  mov rax, %a" pp_dest left_dest;
           printfn ppf "  mov r8, %a" pp_dest right_dest;
           printfn ppf "  cmp rax, r8";
           let eq_lab = Printf.sprintf "lab_%d" (gensym ()) in
           let exit_lab = Printf.sprintf "lab_%d" (gensym ()) in
           printfn ppf "  je %s" eq_lab;
           printfn ppf "  mov qword %a, 0" pp_dest dest;
           printfn ppf "  jmp %s" exit_lab;
           printfn ppf "%s:" eq_lab;
           printfn ppf "  mov qword %a, 1" pp_dest dest;
           printfn ppf "  jmp %s" exit_lab;
           printfn ppf "%s:" exit_lab;
           dealloc_var ppf right_name;
           dealloc_var ppf left_name *)
    | CApp (APrimitive "-", AVar vname, [ AConst (PConst_int 1) ]) ->
      (match is_toplevel vname with
       | None ->
         printfn ppf "  mov qword r11, %a" Addr_of_local.pp_local_exn vname;
         printfn ppf "  dec r11";
         printfn ppf "  mov qword %a, r11" pp_dest dest
       | Some _ ->
         (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
         failwiths "not implemented %d" __LINE__)
    | CApp (APrimitive "-", AVar vname, [ AConst (PConst_int n) ]) ->
      (match is_toplevel vname with
       | None ->
         printfn ppf "  mov qword r11, %a" Addr_of_local.pp_local_exn vname;
         printfn ppf "  sub r11, %d" n;
         printfn ppf "  mov qword %a, r11" pp_dest dest
       | Some _ ->
         (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
         failwiths "not implemented %d" __LINE__)
    | CApp (APrimitive (("+" | "*") as prim), AVar vname, [ AConst (PConst_int n) ])
    | CApp (APrimitive (("+" | "*") as prim), AConst (PConst_int n), [ AVar vname ]) ->
      (match is_toplevel vname with
       | None ->
         printfn ppf "  mov qword r11, %a" Addr_of_local.pp_local_exn vname;
         printfn
           ppf
           "  %s r11, %d"
           (match prim with
            | "+" -> "add "
            | "*" -> "imul"
            | op -> Format.asprintf ";;; TODO %s. %s %d" op __FUNCTION__ __LINE__)
           n;
         printfn ppf "  mov qword %a, r11" pp_dest dest
       | Some _ ->
         (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
         failwiths "not implemented %d" __LINE__)
    | CApp (APrimitive (("+" | "*" | "-") as prim), AVar vl, [ AVar vr ]) ->
      printfn ppf "  mov qword r11, %a" Addr_of_local.pp_local_exn vl;
      printfn ppf "  mov qword r12, %a" Addr_of_local.pp_local_exn vr;
      printfn
        ppf
        "  %s r11, r12"
        (match prim with
         | "+" -> "add "
         | "*" -> "imul"
         | "-" -> "sub"
         | op -> failwiths "not_implemeted  %S. %d" op __LINE__);
      printfn ppf "  mov %a, r11" pp_dest dest
      (* TODO: Maybe move this specialization to the case below  *)
    | CApp (AVar f, arg1, args) when Option.is_some (is_toplevel f) ->
      (* Callig a rukaml function uses custom calling convention.
           CDECL convention: all arguments on stack, LTR *)
      let expected_arity = Option.get (is_toplevel f) in
      let formal_arity = 1 + List.length args in
      (* printfn ppf "\t; expected_arity = %d\n\t; formal_arity = %d"
             expected_arity formal_arity;
           printfn ppf "\t; calling %S" f; *)
      if expected_arity = formal_arity
      then (
        let to_remove = allocate_args (arg1 :: args) in
        printfn ppf "  call %a" Ident.pp f;
        printfn ppf "  add rsp, 8*%d ; dealloc args" to_remove;
        printfn ppf "  mov %a, rax" pp_dest dest)
      else if formal_arity < expected_arity
      then (
        let partial_args_count = allocate_args (arg1 :: args) in
        printfn ppf "  mov rdi, %a" Ident.pp f;
        printfn ppf "  mov rsi, %d" expected_arity;
        printfn ppf "  call rukaml_alloc_closure";
        printfn ppf "  mov rdi, rax";
        printfn ppf "  mov rsi, %d" formal_arity;
        assert (formal_arity < 5);
        (* See calling convention *)
        List.iteri
          (fun i rname -> printfn ppf "  mov %s, [rsp+8*%d]" rname (formal_arity - i - 1))
          (list_take formal_arity [ "rdx"; "rcx"; "r8"; "r9" ]);
        printfn ppf "  mov al, 0";
        printfn ppf "  call rukaml_applyN";
        printfn
          ppf
          "  add rsp, 8*%d ; deallocate args of rukaml_applyN"
          partial_args_count;
        printfn ppf "  mov %a, rax" pp_dest dest
        (* printfn ppf "  sub rsp, 8*2 ; deallocate closure value and padding" *))
      else failwith "Arity mismatch: over application"
    | CApp (AVar f, (AConst _ as arg), []) | CApp (AVar f, (AVar _ as arg), []) ->
      assert (Option.is_none (is_toplevel f));
      let arg =
        match arg with
        | AVar id when id.Frontend.Ident.hum_name = "closure_count" ->
          ANF.AConst (PConst_int 0)
        | _ -> arg
      in
      let arg1 = Ident.of_string "arg1" in
      let temp_padding = Ident.of_string "temp_padding" in
      Addr_of_local.extend temp_padding;
      Addr_of_local.extend arg1;
      printfn ppf "  sub rsp, 8 ; padding";
      printfn ppf "  sub rsp, 8 ; first arg of a function %a" Ident.pp f;
      helper_a (DStack_var arg1) arg;
      printfn ppf "  mov rax, 0  ; no float arguments";
      printfn ppf "  mov rdi, %a" pp_dest (DStack_var f);
      printfn ppf "  mov rsi, 1";
      printfn ppf "  mov rdx, %a" pp_dest (DStack_var arg1);
      printfn ppf "  call rukaml_applyN";
      Addr_of_local.remove_local arg1;
      Addr_of_local.remove_local temp_padding;
      printfn ppf "  add rsp, 8*2 ; free space for args of function \"%a\"" Ident.pp f;
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive "field", AConst (PConst_int n), [ (AVar _ as cont) ]) ->
      helper_a (DReg "rsi") cont;
      printfn ppf "  mov rdi, %d" n;
      printfn ppf "  call rukaml_field";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (AVar id, AUnit, []) when id.Frontend.Ident.hum_name = "gc_compact" ->
      printfn ppf "  mov rdi, rsp";
      printfn ppf "  mov rsi, 0";
      printfn ppf "  call rukaml_gc_compact"
    | CApp (AVar id, AUnit, []) when id.Frontend.Ident.hum_name = "gc_stats" ->
      printfn ppf "  mov rdi, 0";
      printfn ppf "  mov rsi, 0";
      printfn ppf "  call rukaml_gc_print_stats"
    | CApp (AVar id, AUnit, []) when id.Frontend.Ident.hum_name = "closure_count" ->
      printfn ppf "  mov rdi, 0";
      printfn ppf "  mov rsi, 0";
      printfn ppf "  call rukaml_print_alloc_closure_count"
    | CAtom atom -> helper_a dest atom
    | CApp _ as anf ->
      Format.eprintf "Unsupported: @[`%a`@]\n%!" Compile_lib.ANF.pp_c anf;
      failwiths "Not implemented %d" __LINE__
    | rest ->
      printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__;
      printfn ppf "; @[<h>%a@]" Compile_lib.ANF.pp_c rest
  and helper_a (dest : dest) x =
    (* log "  %s: dest=`%a`, expr = %a" __FUNCTION__ pp_dest dest ANF.pp_a x; *)
    match x with
    | AConst (Frontend.Parsetree.PConst_bool true) ->
      printfn ppf "  mov qword %a, 1" pp_dest dest
    | AConst (Frontend.Parsetree.PConst_int n) ->
      printfn ppf "  mov qword %a,  %d" pp_dest dest n
    | AVar ({ Ident.hum_name = "print"; _ } as v) when None = is_toplevel v ->
      alloc_closure ppf (Ident.of_string "rukaml_print_int") 1;
      printfn ppf "  mov %a, rax" pp_dest dest
    | AVar vname ->
      (match is_toplevel vname with
       | None ->
         printfn
           ppf
           "  mov qword rdx, %a ; use temp rdx to move from stack to stack"
           Addr_of_local.pp_local_exn
           vname;
         printfn
           ppf
           "  mov qword %a, rdx ; access a var \"%a\""
           pp_dest
           dest
           Ident.pp
           vname
       | Some arity ->
         alloc_closure ppf vname arity;
         printfn ppf "  mov %a, rax" pp_dest dest)
    | ATuple (a, b, []) ->
      helper_a (DReg "rdi") a;
      helper_a (DReg "rsi") b;
      printfn ppf "  call rukaml_alloc_pair";
      printfn ppf "  mov %a, rax" pp_dest dest
    | AUnit -> printfn ppf "mov qword %a, 0" pp_dest dest
    | atom ->
      printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__;
      printfn ppf ";;; @[`%a`@]" ANF.pp_a atom
  in
  let dealloc_locals = allocate_locals ppf body in
  helper (DReg "rax") body;
  dealloc_locals ~now:()
;;

let put_print_newline ppf =
  printfn
    ppf
    {|print_newline:
          mov rax, 1 ; 'write' syscall identifier
          mov rdi, 1 ; stdout file descriptor
          mov rsi, newline_char ; where do we take data from
          mov rdx, 1 ; the amount of bytes to write
          syscall
          ret |}
;;

let put_print_hex ppf =
  printfn
    ppf
    {|
print_hex:
  mov rax, rdi
  mov rdi, 1
  mov rdx, 1
  mov rcx, 64 ; how far are we shifting rax?
iterate:
  push rax ; Save the initial rax value
  sub rcx, 4
  sar rax, cl ; shift to 60, 56, 52, ... 4, 0
              ; the cl register is the smallest part of rcx
  and rax, 0xf ; clear all bits but the lowest four
  lea rsi, [codes + rax]; take a hexadecimal digit character code
  mov rax, 1
  push rcx  ; syscall will break rcx
  syscall   ; rax = 1 (31) -- the write identifier,
            ; rdi = 1 for stdout,
            ; rsi = the address of a character, see line 29
  pop rcx
  pop rax          ; see line 24
  test rcx, rcx    ; rcx = 0 when all digits are shown
  jnz iterate
  ret
|}
;;

let use_custom_main = false

let codegen ?(wrap_main_into_start = true) anf file =
  (* log "Going to generate code here %s %d" __FUNCTION__ __LINE__; *)
  log "ANF: @[%a@]" Compile_lib.ANF.pp_stru anf;
  let is_toplevel =
    let hash = Hashtbl.create (List.length anf) in
    List.iter
      (fun (_, name, body) ->
         let pats, _ = Compile_lib.ANF.group_abstractions body in
         let argc = List.length pats in
         assert (argc >= 1 || name.Ident.hum_name = "main");
         Hashtbl.add hash name argc)
      anf;
    fun name ->
      match Hashtbl.find hash name with
      | n -> Some n
      | exception Not_found -> None
  in
  Stdio.Out_channel.with_file file ~f:(fun ch ->
    let ppf = Format.formatter_of_out_channel ch in
    printfn ppf "section .note.GNU-stack noalloc noexec nowrite progbits";
    if use_custom_main
    then
      printfn
        ppf
        {|section .data
            newline_char: db 10
            codes: db '0123456789abcdef' |};
    printfn ppf "section .text";
    if use_custom_main
    then (
      put_print_newline ppf;
      put_print_hex ppf);
    (* externs *)
    List.iter
      (printfn ppf "extern %s")
      [ "rukaml_alloc_closure"
      ; "rukaml_print_int"
      ; "rukaml_applyN"
      ; "rukaml_field"
      ; (* printfn ppf "extern rukaml_alloc_tuple"; *)
        "rukaml_alloc_pair"
      ; "rukaml_initialize"
      ; "rukaml_gc_compact"
      ; "rukaml_gc_print_stats"
      ; "rukaml_print_alloc_closure_count"
      ];
    printfn ppf "";
    if use_custom_main
    then
      (* TODO: use exit_group syscall (231)
           https://filippo.io/linux-syscall-table/ *)
      printfn
        ppf
        {|_start:
              push    rbp
              mov     rbp, rsp   ; prologue
              push 5
              call sq
              add rsp, 8
              mov rdi, rax    ; rdi stores return code
              mov rax, 60     ; exit syscall
              syscall|}
    else if wrap_main_into_start
    then
      printfn
        ppf
        {|_start:
              push    rbp
              mov     rbp, rsp   ; prologue
              call main
              mov rdi, rax    ; rdi stores return code
              mov rax, 60     ; exit syscall
              syscall|};
    let open Compile_lib in
    anf
    |> List.iter (fun (_flg, name, expr) ->
      if Addr_of_local.size () <> 0
      then
        failwiths
          "There are left over variables (before function %a): %s "
          Ident.pp
          name
          (Addr_of_local.keys ());
      (* printfn ppf "";
                fprintf ppf "\t; %a\n" Loc_of_ident.pp (); *)

      (* print_prologue ppf name; *)

      (* fprintf ppf "  ; There are %d known arguments in %s\n%!"
                (Loc_of_ident.size ()) name; *)

      (* printfn ppf "  sub rsp, %d" (8 * Loc_of_ident.size ()); *)
      if use_custom_main && name.Ident.hum_name = "main"
      then
        printfn
          ppf
          {|_start:
                    push    rbp
                    mov     rbp, rsp   ; prologue
                    push 5
                    call double
                    add rsp, 8 ; pop 5
                    mov rdi, rax
                    call print_hex
                    call print_newline

                    mov rdi, 0x1122334455667788
                    call print_hex
                    call print_newline
                    mov rax, 60
                    xor rdi, rdi
                    syscall|}
      else (
        let () = printfn ppf "GLOBAL %a" Ident.pp name in
        let () = printfn ppf "@[<h>%a:@]" Ident.pp name in
        let pats, body = ANF.group_abstractions expr in
        let argc = List.length pats in
        let names =
          List.map
            (function
              | ANF.APname name -> name)
            pats
        in
        List.rev pats
        |> ListLabels.iteri ~f:(fun i -> function
          | ANF.APname name -> Addr_of_local.add_arg ~argc i name);
        printfn ppf "  push rbp";
        printfn ppf "  mov  rbp, rsp";
        if name.hum_name = "main"
        then (
          printfn ppf "  mov rdi, rsp";
          printfn ppf "  call rukaml_initialize");
        generate_body is_toplevel ppf body;
        Addr_of_local.remove_args names;
        print_epilogue ppf name.hum_name);
      ());
    Format.pp_print_flush ppf ());
  Result.Ok ()
;;
