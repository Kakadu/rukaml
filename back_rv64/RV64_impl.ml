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

(*
   How to debug:
     qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 -g 1234 tests/rv64/fac.exe
     gdb-multiarch ./fac.exe
*)

let failwiths fmt = Format.kasprintf failwith fmt

type config = { mutable verbose : bool }

let cfg = { verbose = false }

let log fmt =
  if cfg.verbose then Format.kasprintf (Format.printf "%s\n%!") fmt
  else Format.ifprintf Format.std_formatter fmt

let set_verbose b =
  (* Printf.printf "verbosity in %s = %b\n%!" __FILE__ b; *)
  cfg.verbose <- b

let fprintf = Format.fprintf
let printfn ppf fmt = Format.kfprintf (fun ppf -> fprintf ppf "\n") ppf fmt

let pp_space_list eta =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") eta

(* let print_prologue ppf name =
   if name = "main" then (
     printfn ppf "global _start";
     printfn ppf "_start:")
   else printfn ppf "_%s:" name;
   (* printfn ppf "%s:" name; *)
   printfn ppf "  push rbp";
   printfn ppf "  mov  rbp, rsp";
   (* movq dst, src *)
   (* printfn ppf "  ;sub rsp, 24 ; given 24 is total size of local variables"; *)
   fprintf ppf "%!"
*)
let print_epilogue ppf fname =
  (* printfn ppf "  pop rbp"; *)
  if fname <> "main" then printfn ppf "  ret  # %s" fname
  else (
    printfn ppf "  addi    a0, x0, 0   # Use 0 return code";
    printfn ppf "  addi    a7, x0, 93  # Service command code 93 terminates";
    printfn ppf "  ecall               # Call linux to terminate the program");
  fprintf ppf "%!"

module ANF = Compile_lib.ANF2

let gensym =
  let open ANF in
  reset_gensym ();
  gensym

let list_take_n n xs =
  let rec helper n forw xs =
    if n = 0 then (List.rev forw, xs)
    else
      match xs with
      | [] -> failwith "bad argument"
      | h :: tl -> helper (n - 1) (h :: forw) tl
  in
  helper n [] xs

let list_take n xs =
  (* TODO: it's not optimal *)
  fst (list_take_n n xs)

type dest = DReg of string | DStack_var of string

module Addr_of_local = struct
  let store = Hashtbl.create 13
  let last_pos = ref 0
  let get_locals_count () = !last_pos

  let clear () =
    Hashtbl.clear store;
    last_pos := 0

  let extend name =
    incr last_pos;
    (* log "extend %s with shift = %d" name !last_pos; *)
    Hashtbl.add store name !last_pos

  let remove_local name =
    let pos = Hashtbl.find store name in
    if !last_pos = pos then (
      (* log "remove %s with shift = %d" name !last_pos; *)
      decr last_pos;
      Hashtbl.remove store name)
    else
      failwiths "Something bad %d. Can't remove local variable %S" __LINE__ name

  let count () = Hashtbl.length store
  let size = count
  let contains name = Hashtbl.mem store name
  let has_key = contains

  let find_exn name =
    match Hashtbl.find store name with
    | v -> v
    | exception Not_found ->
        failwiths "Can't find location of a variable %S" name

  let lookup_exn = find_exn

  let add_arg ~argc i name =
    assert (i < argc);
    let loc = 1 - argc + i in
    assert (loc <= 0);
    log "Location argument %S in [rbp+%d]" name (-loc);
    Hashtbl.add store name loc

  let remove_args xs =
    log "Removing info about args [ %a ]"
      (pp_space_list Format.pp_print_string)
      xs;
    List.iter (Hashtbl.remove store) xs

  let pp_local_exn ppf name =
    let offset =
      let o = find_exn name in
      if o > 0 then !last_pos - o else !last_pos - o
    in
    if not (offset >= 0) then
      assert (
        (* Format.eprintf "Assertion failed, offset = %d, v=%S\n%!" offset name;
           Format.eprintf "       locals = %d\n%!" locals; *)
        offset >= 0);
    if offset = 0 then fprintf ppf "(sp)" else fprintf ppf "%d(sp)" (offset * 8)
  (* 8 for 64 bit, 4 for 32bit *)
  (* if offset > 0 then fprintf ppf "[rbp-%d*8]" offset
     else fprintf ppf "[rbp+%d*8]" (-offset) *)

  let pp_dest ppf = function
    | DReg s -> fprintf ppf "%s" s
    | DStack_var name -> pp_local_exn ppf name

  let keys () =
    Hashtbl.to_seq_keys store |> Seq.fold_left (fun acc x -> acc ^ " " ^ x) ""
end

let allocate_locals ppf input_anf : (now:unit -> unit) * _ =
  let names = ref [] in
  let rec helper = function
    | ANF.EComplex c -> helper_c c
    | ELet (_flg, PVar name, rhs, where_) ->
        Addr_of_local.extend name;
        names := name :: !names;
        helper_c rhs;
        helper where_
    | ELet (_, PTuple (_, _, _), _, _) -> assert false
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
  let args_repr = String.concat ", " !names in
  if count > 0 then
    printfn ppf "  addi sp, sp, -(8*%d) # allocate for local variables %s" count
      args_repr;

  (* let deallocate_padding =
       if count mod 2 = 1 then (
         let pad_name = Printf.sprintf "__pad%d" (gensym ()) in
         printfn ppf "  addi sp, sp, -8 # allocate padding for locals";
         Addr_of_local.extend pad_name;
         fun () ->
           Addr_of_local.remove_local pad_name;
           printfn ppf "  add rsp, 8 # deallocate padding for locals")
       else fun () -> ()
     in *)
  let deallocate =
    if count > 0 then (fun ~now ->
      let () = now in

      (* deallocate_padding (); *)
      printfn ppf "  addi sp, sp, 8*%d # deallocate local variables %s" count
        args_repr;
      !names |> List.iter Addr_of_local.remove_local)
    else fun ~now ->
      let () = now in
      ()
  in
  (deallocate, count)

let print_alloc_closure ppf fname arity =
  printfn ppf "  lla a0, %s" fname;
  printfn ppf "  li a1, %d" arity;
  printfn ppf "  call rukaml_alloc_closure"

let list_iter_revindex ~f xs =
  let l = List.length xs in
  List.iteri (fun n x -> f (l - n - 1) x) xs

let store_ra_temp ppf f =
  let ra_temp_name = Printf.sprintf "temp_ra_%d" (gensym ()) in
  Addr_of_local.extend ra_temp_name;
  printfn ppf "  addi sp, sp, -8 # alloc space for RA register";
  (* printfn ppf "  sd ra, (sp)"; *)
  let rez = f ra_temp_name in
  printfn ppf "  ld ra, (sp)";
  printfn ppf "  addi sp, sp, 8 # free space of RA register";
  Addr_of_local.remove_local ra_temp_name;
  rez

(**
    Argument [is_toplevel] returns None or Some arity. *)
let generate_body is_toplevel ppf body =
  let open Miniml.Parsetree in
  let dealloc_locals, locals = allocate_locals ppf body in
  let deallocate_args_for_call argc =
    Addr_of_local.(last_pos := !last_pos - argc);
    printfn ppf "  addi sp, sp, 8*%d # deallocate %d args" argc argc
  in
  let allocate_args_for_call ?f args =
    (* Allocate args for a function call inside a body *)
    let count = List.length args in

    printfn ppf "  # Allocate args to call fun %S arguments" (Option.get f);
    ListLabels.iteri args ~f:(fun i ->
        let pp_access ?(doc = "") v =
          printfn ppf "  li t0, %d" v;
          Format.fprintf ppf "  sd t0, (sp)";
          if doc <> "" then printfn ppf " # %s" doc else printfn ppf ""
        in
        printfn ppf "  addi sp, sp, -8 #";
        incr Addr_of_local.last_pos;
        function
        | Compile_lib.ANF2.AUnit | AConst (PConst_bool false) -> pp_access 0
        | AConst (PConst_bool true) -> pp_access 1
        | AConst (PConst_int n) -> pp_access ~doc:"constant" n
        | AVar vname when Option.is_some (is_toplevel vname) -> (
            match is_toplevel vname with
            | Some arity ->
                store_ra_temp ppf (fun ra_name ->
                    printfn ppf "  lla a0, %s" vname;
                    printfn ppf "  li a1, %d" arity;
                    printfn ppf "  sd ra, %a" Addr_of_local.pp_local_exn ra_name;
                    printfn ppf "  call rukaml_alloc_closure"
                    (* print_alloc_closure ppf vname arity *));
                printfn ppf "  mov qword [rsp%+d*8], rax # arg %S" i vname
            | None -> assert false)
        | AVar vname ->
            printfn ppf "  ld t0, %a  # arg %S" Addr_of_local.pp_local_exn vname
              vname;
            printfn ppf "  sd t0, (sp)"
        | ALam _ -> failwith "Should it be representable in ANF?"
        | APrimitive _ -> assert false
        | ATuple _ -> assert false);
    (* printfn ppf "  addi sp, sp, -8*%d # fun %S arguments" count (Option.get f); *)
    count
  in

  let rec helper dest = function
    | Compile_lib.ANF2.EComplex c -> helper_c dest c
    | ELet (_, Miniml.Parsetree.PVar name, rhs, wher) ->
        assert (Addr_of_local.contains name);
        let local = DStack_var name in
        (* printfn ppf "    ;; calculate rhs and put into %a. offset = %d" pp_dest
           dest
           (Addr_of_local.find_exn name); *)
        helper_c local rhs;
        helper dest wher
    | ELet (_, PTuple (_, _, _), _, _) -> assert false
  and helper_c (dest : dest) = function
    | CIte (AConst (Miniml.Parsetree.PConst_bool true), bth, _bel) ->
        helper dest bth
    | CIte (AConst (Miniml.Parsetree.PConst_bool false), _bth, bel) ->
        helper dest bel
    | CIte (AVar econd, bth, bel) when Addr_of_local.contains econd ->
        (* if on global or local variable  *)
        printfn ppf "  ld  t0, %a" Addr_of_local.pp_local_exn econd;
        let el_lab = Printf.sprintf "lab_else_%d" (gensym ()) in
        let fin_lab = Printf.sprintf "lab_endif_%d" (gensym ()) in
        printfn ppf "  beq t0, zero, %s" el_lab;
        helper dest bth;
        printfn ppf "  beq zero, zero, %s" fin_lab;
        printfn ppf "%s:  # %s is 0 " el_lab econd;
        helper dest bel;
        printfn ppf "%s:" fin_lab
    | CApp (AVar ("print" as f), arg1, [])
      when is_toplevel f = None && not (Addr_of_local.has_key f) -> (
        match arg1 with
        | AVar v when Addr_of_local.has_key v ->
            store_ra_temp ppf (fun ra_name ->
                printfn ppf "  ld a0, %a" Addr_of_local.pp_local_exn v;
                printfn ppf "  sd ra, %a" Addr_of_local.pp_local_exn ra_name;
                printfn ppf "  call rukaml_print_int";
                printfn ppf "  sd a0, %a" Addr_of_local.pp_dest dest)
        | AConst (PConst_int n) ->
            store_ra_temp ppf (fun ra_name ->
                printfn ppf "  li a0, %d" n;
                printfn ppf "  sd ra, %a" Addr_of_local.pp_local_exn ra_name;
                printfn ppf "  call rukaml_print_int";
                printfn ppf "  sd a0, %a" Addr_of_local.pp_dest dest)
        | AConst (PConst_bool _)
        | AVar _ | APrimitive _
        | ATuple (_, _, _)
        | ALam (_, _)
        | AUnit ->
            failwith "Should not happen")
    | CApp (APrimitive "=", AConst (PConst_int l), [ AConst (PConst_int r) ]) ->
        (* TODO: user Addr_of_local.pp_local_exn *)
        if l = r then printfn ppf "  mov qword %a, 1" Addr_of_local.pp_dest dest
        else printfn ppf "  mov qword %a, 0" Addr_of_local.pp_dest dest
    | CApp (APrimitive ("=" as op), AConst (PConst_int n), [ AVar vname ])
    | CApp
        (APrimitive (("=" | "<") as op), AVar vname, [ AConst (PConst_int n) ])
      ->
        let branch_instr =
          match op with "=" -> "beq" | "<" -> "blt" | _ -> assert false
        in
        let eq_lab = Printf.sprintf "lab_%d" (gensym ()) in
        let exit_lab = Printf.sprintf "lab_%d" (gensym ()) in
        printfn ppf " # %a, find_exn %S = %d, last_pos = %d  "
          Addr_of_local.pp_local_exn vname vname
          (Addr_of_local.find_exn vname)
          !Addr_of_local.last_pos;
        printfn ppf "  ld t0, %a # locals = %d" Addr_of_local.pp_local_exn vname
          locals;
        printfn ppf "  li t1, %d" n;
        printfn ppf "  %s t0, t1, %s" branch_instr eq_lab;
        (* TODO: user Addr_of_local.pp_local_exn *)
        printfn ppf "  sd zero, %a" Addr_of_local.pp_dest dest;
        printfn ppf "  beq zero, zero, %s # Where is unconditional jump?"
          exit_lab;
        printfn ppf "%s:" eq_lab;
        printfn ppf "  li t0, 1 # (* RISC is weird *)";
        printfn ppf "  sd t0, %a # dest = %a" Addr_of_local.pp_dest dest
          Addr_of_local.pp_dest dest;
        printfn ppf "  beq zero, zero, %s # not needed?" exit_lab;
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
    | CApp (APrimitive "-", AVar vname, [ AConst (PConst_int 1) ]) -> (
        match is_toplevel vname with
        | None ->
            printfn ppf "  ld t0, %a" Addr_of_local.pp_local_exn vname;
            printfn ppf "  addi t0, t0, -1";
            printfn ppf "  sd t0, %a" Addr_of_local.pp_dest dest
        | Some _ ->
            (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
            failwiths "not implemented %d" __LINE__)
    | CApp (APrimitive "-", AVar vname, [ AConst (PConst_int n) ]) -> (
        match is_toplevel vname with
        | None ->
            printfn ppf "  ld t5, %a #" Addr_of_local.pp_local_exn vname;
            printfn ppf "  addi t5, t5, -%d" n;
            printfn ppf "  sd t5, %a" Addr_of_local.pp_dest dest
        | Some _ ->
            (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
            failwiths "not implemented %d" __LINE__)
    | CApp
        (APrimitive (("+" | "*") as prim), AVar vname, [ AConst (PConst_int n) ])
    | CApp
        (APrimitive (("+" | "*") as prim), AConst (PConst_int n), [ AVar vname ])
      -> (
        match is_toplevel vname with
        | None ->
            printfn ppf "  mov qword r11, %a #" Addr_of_local.pp_local_exn vname;
            printfn ppf "  %s r11, %d"
              (match prim with
              | "+" -> "add "
              | "*" -> "imul"
              | op ->
                  Format.asprintf ";;; TODO %s. %s %d" op __FUNCTION__ __LINE__)
              n;
            printfn ppf "  mov qword %a, r11" Addr_of_local.pp_dest dest
        | Some _ ->
            (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
            failwiths "not implemented %d" __LINE__)
    | CApp (APrimitive (("+" | "*" | "-") as prim), AVar vl, [ AVar vr ]) -> (
        printfn ppf "# %s is stored in %d" vl (Addr_of_local.find_exn vl);
        printfn ppf "# %s is stored in %d" vr (Addr_of_local.find_exn vr);
        printfn ppf "# last_pos = %d" !Addr_of_local.last_pos;
        printfn ppf "  ld t3, %a #" Addr_of_local.pp_local_exn vl;
        printfn ppf "  ld t4, %a" Addr_of_local.pp_local_exn vr;
        printfn ppf "  %s t5, t3, t4"
          (match prim with
          | "+" -> "add "
          | "*" -> "mulw"
          | "-" -> "sub"
          | op -> failwiths "not_implemeted  %S. %d" op __LINE__);
        match dest with
        | DReg _ -> printfn ppf "  addi %a, t5, 0" Addr_of_local.pp_dest dest
        | DStack_var _ -> printfn ppf "  sd t5, %a" Addr_of_local.pp_dest dest
        (* TODO: Maybe move this specialization to the case below  *))
    | CApp (AVar f, arg1, args) when Option.is_some (is_toplevel f) ->
        (* Callig a rukaml function uses custom calling convention.
           Pascal convention: all arguments on stack, LTR *)
        let expected_arity = Option.get (is_toplevel f) in
        let formal_arity = 1 + List.length args in

        (* printfn ppf "\t; expected_arity = %d\n\t; formal_arity = %d"
             expected_arity formal_arity;
           printfn ppf "\t; calling %S" f; *)
        if expected_arity = formal_arity then
          let _ =
            store_ra_temp ppf (fun ra_name ->
                let to_remove = allocate_args_for_call ~f (arg1 :: args) in

                printfn ppf "  sd ra, %a" Addr_of_local.pp_local_exn ra_name;
                printfn ppf "  call %s" f;
                deallocate_args_for_call to_remove;
                to_remove)
          in

          (* incr Addr_of_local.last_pos;
             printfn ppf "  addi sp, sp, -8";
             printfn ppf "  sd ra, (sp)";
             printfn ppf "  call %s" f;
             printfn ppf "  ld ra, (sp)";
             printfn ppf "  addi sp, sp, 8";
             decr Addr_of_local.last_pos; *)
          printfn ppf "  sd a0, %a" Addr_of_local.pp_dest dest
        else if formal_arity < expected_arity then (
          store_ra_temp ppf (fun ra_name ->
              printfn ppf "  lla a0, %s" f;
              printfn ppf "  li a1, %d" expected_arity;
              printfn ppf "  sd ra, %a" Addr_of_local.pp_local_exn ra_name;
              printfn ppf "  call rukaml_alloc_closure");

          (* printfn ppf "  add0 a0, a0, 0"; *)
          let partial_args_count = allocate_args_for_call ~f (arg1 :: args) in
          let () =
            store_ra_temp ppf (fun ra_name ->
                printfn ppf "  li a1, %d" formal_arity;
                assert (formal_arity < 5);
                (* See calling convention *)
                List.iteri
                  (fun i rname ->
                    printfn ppf "  ld %s, %+d*8(sp) # arg %d" rname
                      (formal_arity - i) i)
                  (list_take formal_arity [ (*"a0"; *) "a2"; "a3"; "a4"; "a5" ]);

                (* printfn ppf "  mov al, 0"; *)
                printfn ppf "  sd ra, %a" Addr_of_local.pp_local_exn ra_name;
                printfn ppf "  call rukaml_applyN";

                (* printfn ppf "  call rukaml_applyN"; *)
                printfn ppf "  sd a0, %a" Addr_of_local.pp_dest dest
                (* Needed because we allocate temporary space to prepare arguments  *)

                (* printfn ppf
                   "  addi sp, sp, 8*%d # deallocate args of rukaml_applyN"
                   partial_args_count *))
          in
          deallocate_args_for_call formal_arity
          (* printfn ppf "  sub rsp, 8*2 ; deallocate closure value and padding" *))
        else failwith "Arity mismatch: over application"
    | CApp (AVar f, (AConst _ as arg), []) | CApp (AVar f, (AVar _ as arg), [])
      ->
        assert (Option.is_none (is_toplevel f));
        Addr_of_local.extend "arg1";
        printfn ppf "  addi sp, sp, -8 #first arg of a function %s" f;
        helper_a (DStack_var "arg1") arg;
        store_ra_temp ppf (fun ra_name ->
            (* printfn ppf "  mov rax, 0  ; no float arguments"; *)
            printfn ppf "  ld a0, %a" Addr_of_local.pp_dest (DStack_var f);
            printfn ppf "  li a1, 1";
            printfn ppf "  ld a2, %a" Addr_of_local.pp_dest (DStack_var "arg1");
            printfn ppf "  sd ra, %a" Addr_of_local.pp_local_exn ra_name;
            printfn ppf "  call rukaml_applyN");
        Addr_of_local.remove_local "arg1";
        (* Addr_of_local.remove_local "temp_padding"; *)
        printfn ppf "  addi sp, sp, 8 # free space for args of function %S" f;
        if dest <> DReg "a0" then
          printfn ppf "  sd a0, %a" Addr_of_local.pp_dest dest
    | CApp (APrimitive "field", AConst (PConst_int n), [ (AVar _ as cont) ]) ->
        helper_a (DReg "rsi") cont;
        printfn ppf "  mov rdi, %d" n;
        printfn ppf "  call rukaml_field";
        printfn ppf "  mov %a, rax" Addr_of_local.pp_dest dest
    | CApp (AVar "gc_compact", AUnit, []) ->
        printfn ppf "  mov rdi, rsp";
        printfn ppf "  mov rsi, 0";
        printfn ppf "  call rukaml_gc_compact"
    | CApp (AVar "gc_stats", AUnit, []) ->
        printfn ppf "  mov rdi, 0";
        printfn ppf "  mov rsi, 0";
        printfn ppf "  call rukaml_gc_print_stats"
    | CAtom atom -> helper_a dest atom
    | CApp _ as anf ->
        Format.eprintf "Unsupported: @[`%a`@]\n%!" Compile_lib.ANF2.pp_c anf;
        failwiths "Not implemented %d" __LINE__
    | rest ->
        printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__;
        printfn ppf "; @[<h>%a@]" Compile_lib.ANF2.pp_c rest
  and helper_a (dest : dest) x =
    (* log "  %s: dest=`%a`, expr = %a" __FUNCTION__ pp_dest dest ANF.pp_a x; *)
    match x with
    | AConst (Miniml.Parsetree.PConst_bool true) ->
        printfn ppf "  li %a, 1" Addr_of_local.pp_dest dest
    | AConst (Miniml.Parsetree.PConst_int n) -> (
        match dest with
        | DReg _ -> printfn ppf "  li %a, %d" Addr_of_local.pp_dest dest n
        | DStack_var _ ->
            printfn ppf "  li t0, %d" n;
            printfn ppf "  sd t0, %a" Addr_of_local.pp_dest dest)
    | AVar vname -> (
        match is_toplevel vname with
        | None -> (
            printfn ppf "  ld t5, %a" Addr_of_local.pp_local_exn vname;
            match dest with
            | DReg _ ->
                printfn ppf "  addi %a, t5, 0 # 24" Addr_of_local.pp_dest dest
            | DStack_var _ ->
                printfn ppf "  sd t5, %a # access a var %S"
                  Addr_of_local.pp_dest dest vname)
        | Some arity ->
            print_alloc_closure ppf vname arity;
            printfn ppf "  sd a0, %a" Addr_of_local.pp_dest dest)
    | ATuple (a, b, []) ->
        store_ra_temp ppf (fun ra_name ->
            helper_a (DReg "r0") a;
            helper_a (DReg "r1") b;

            printfn ppf "  sd ra, %a" Addr_of_local.pp_local_exn ra_name;
            printfn ppf "  call rukaml_alloc_pair";
            printfn ppf "  mv %a, a0" Addr_of_local.pp_dest dest)
    | AUnit -> printfn ppf "mov qword %a, 0" Addr_of_local.pp_dest dest
    | atom ->
        printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__;
        printfn ppf ";;; @[`%a`@]" ANF.pp_a atom
  in

  helper (DReg "a0") body;
  dealloc_locals ~now:()

let put_print_newline ppf =
  printfn ppf
    {|print_newline:
          mov rax, 1 ; 'write' syscall identifier
          mov rdi, 1 ; stdout file descriptor
          mov rsi, newline_char ; where do we take data from
          mov rdx, 1 ; the amount of bytes to write
          syscall
          ret |}

let put_print_hex ppf =
  printfn ppf
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

let use_custom_main = false

let codegen ?(wrap_main_into_start = true) anf file =
  (* log "Going to generate code here %s %d" __FUNCTION__ __LINE__; *)
  log "ANF: @[%a@]" Compile_lib.ANF2.pp_stru anf;

  let is_toplevel =
    let hash = Hashtbl.create (List.length anf) in
    List.iter
      (fun (_, name, body) ->
        let pats, _ = Compile_lib.ANF2.group_abstractions body in
        let argc = List.length pats in
        assert (argc >= 1 || name = "main");
        Hashtbl.add hash name argc)
      anf;

    fun name ->
      match Hashtbl.find hash name with
      | n -> Some n
      | exception Not_found -> None
  in

  Stdio.Out_channel.with_file file ~f:(fun ch ->
      let ppf = Format.formatter_of_out_channel ch in

      (* printfn ppf "section .note.GNU-stack noalloc noexec nowrite progbits"; *)
      if use_custom_main then
        printfn ppf
          {|section .data
            newline_char: db 10
            codes: db '0123456789abcdef' |};

      (* printfn ppf "section .text"; *)
      if use_custom_main then (
        put_print_newline ppf;
        put_print_hex ppf);

      (* externs *)
      let __ () =
        List.iter (printfn ppf "extern %s")
          [
            "rukaml_alloc_closure";
            "rukaml_print_int";
            "rukaml_applyN";
            "rukaml_field";
            (* printfn ppf "extern rukaml_alloc_tuple"; *)
            "rukaml_alloc_pair";
            "rukaml_initialize";
            "rukaml_gc_compact";
            "rukaml_gc_print_stats";
          ];
        printfn ppf ""
      in
      (* if use_custom_main then
           (* TODO: use exit_group syscall (231)
              https://filippo.io/linux-syscall-table/ *)
           printfn ppf
             {|_start:
                 push    rbp
                 mov     rbp, rsp   ; prologue
                 push 5
                 call sq
                 add rsp, 8
                 mov rdi, rax    ; rdi stores return code
                 mov rax, 60     ; exit syscall
                 syscall|}
         else *)
      if wrap_main_into_start then
        printfn ppf
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
             if Addr_of_local.size () <> 0 then
               failwiths
                 "There are left over variables (before function %s): %s " name
                 (Addr_of_local.keys ());

             (* printfn ppf "";
                fprintf ppf "\t; %a\n" Loc_of_ident.pp (); *)

             (* print_prologue ppf name; *)

             (* fprintf ppf "  ; There are %d known arguments in %s\n%!"
                (Loc_of_ident.size ()) name; *)

             (* printfn ppf "  sub rsp, %d" (8 * Loc_of_ident.size ()); *)
             (if false then ()
              else
                let () = printfn ppf ".globl %s" name in
                let () = printfn ppf "@[<h>%s:@]" name in

                let pats, body = ANF2.group_abstractions expr in

                let argc = List.length pats in
                let names =
                  List.map (function ANF2.APname name -> name) pats
                in
                List.rev pats
                |> ListLabels.iteri ~f:(fun i -> function
                     | ANF2.APname name -> Addr_of_local.add_arg ~argc i name);

                (* printfn ppf "  push rbp";
                   printfn ppf "  mov  rbp, rsp"; *)
                (* if name = "main" then (
                   printfn ppf "  mv a0, sp";
                   printfn ppf "  call rukaml_initialize"); *)
                generate_body is_toplevel ppf body;
                Addr_of_local.remove_args names;

                print_epilogue ppf name);
             ());
      Format.pp_print_flush ppf ());

  Result.Ok ()