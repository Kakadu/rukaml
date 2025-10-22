(* https://flint.cs.yale.edu/cs421/papers/x86-asm/asm.html
   https://jvns.ca/blog/2021/05/17/how-to-look-at-the-stack-in-gdb
   https://en.wikipedia.org/wiki/X86_calling_conventions
   https://github.com/jhucompilers/fall2022/tree/gh-pages/lectures
*)

(*
   How to debug:
     qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 -g 1234 tests/rv64/fac.exe
     gdb-multiarch ./fac.exe
*)

let sprintf = Format.sprintf
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

module ANF = Compile_lib.ANF

let gensym =
  let open ANF in
  reset_gensym ();
  gensym
;;

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

open Frontend

type dest =
  | DReg of string
  | DStack_var of Ident.t

(* TODO: Understand diffrence between this and the same module in AMD64 *)
module Addr_of_local = struct
  let store : (Ident.t, _) Hashtbl.t = Hashtbl.create 13
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
      failwiths
        "Something bad %d. Can't remove local variable \"%a\""
        __LINE__
        Ident.pp
        name
  ;;

  let count () = Hashtbl.length store
  let size = count
  let contains name = Hashtbl.mem store name
  let has_key = contains

  let find_exn name =
    match Hashtbl.find store name with
    | v -> v
    | exception Not_found ->
      failwiths "Can't find location of a variable \"%a\"" Ident.pp name
  ;;

  let lookup_exn = find_exn

  let add_arg ~argc i name =
    assert (i < argc);
    let loc = 1 - argc + i in
    assert (loc <= 0);
    log "Location argument \"%a\" in [rbp+%d]" Ident.pp name (-loc);
    Hashtbl.add store name loc
  ;;

  let remove_args xs =
    log "Removing info about args [ %s ]" (Ident.concat_str xs);
    List.iter (Hashtbl.remove store) xs
  ;;

  let pp_local_exn ppf name =
    let offset =
      let o = find_exn name in
      if o > 0 then !last_pos - o else !last_pos - o
    in
    if not (offset >= 0)
    then
      assert (
        (* Format.eprintf "Assertion failed, offset = %d, v=%S\n%!" offset name;
           Format.eprintf "       locals = %d\n%!" locals; *)
        offset
        >= 0);
    if offset = 0 then fprintf ppf "(sp)" else fprintf ppf "%d(sp)" (offset * 8)
  ;;

  (* 8 for 64 bit, 4 for 32bit *)
  (* if offset > 0 then fprintf ppf "[rbp-%d*8]" offset
     else fprintf ppf "[rbp+%d*8]" (-offset) *)

  let pp_to_mach name =
    let offset =
      let o = find_exn name in
      if o > 0 then !last_pos - o else !last_pos - o
    in
    if not (offset >= 0) then assert (offset >= 0);
    Machine.(ROffset (SP, offset * 8))
  ;;

  let pp_dest ppf = function
    | DReg s -> fprintf ppf "%s" s
    | DStack_var name -> pp_local_exn ppf name
  ;;

  let keys () =
    Hashtbl.to_seq_keys store
    |> Seq.fold_left (fun acc x -> Format.asprintf "%s %a" acc Ident.pp x) ""
  ;;

  let pp ppf () =
    Hashtbl.iter
      (fun k _ ->
         Format.fprintf ppf "@[%a ~> %a@],@ " Ident.pp k Machine.pp_reg (pp_to_mach k))
      store;
    Format.pp_print_flush ppf ()
  ;;
end

let list_iter_revindex ~f xs =
  let l = List.length xs in
  List.iteri (fun n x -> f (l - n - 1) x) xs
;;

open Machine

let allocate_locals input_anf : (now:unit -> unit) * _ =
  let __ _ =
    log
      "Allocate locals: last_pos = %d, keys = %s"
      !Addr_of_local.last_pos
      (Addr_of_local.keys ())
  in
  let local_names = ref Ident.Ident_set.empty in
  let rec helper = function
    | ANF.EComplex c -> helper_c c
    | ELet (_flg, Tpat_var name, rhs, where_) ->
      local_names := Ident.Ident_set.add name !local_names;
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
  let local_names = Ident.Ident_set.to_list !local_names in
  let count = List.length local_names in
  (* If assertion fails it's like a number of locals with the same names *)
  let args_repr = Ident.concat_str local_names in
  let ra_offset =
    let comm, sp_offset =
      if count mod 2 = 0
      then sprintf "allocate for Pad, RA, and %d locals %s" count args_repr, count + 2
      else sprintf "allocate for RA, and %d locals %s" count args_repr, count + 1
    in
    assert (sp_offset mod 2 = 0);
    emit addi sp sp (-8 * sp_offset) ~comm;
    Addr_of_local.last_pos := !Addr_of_local.last_pos + sp_offset - count;
    List.iter Addr_of_local.extend (List.rev local_names);
    8 * count
  in
  emit sd ra (ROffset (SP, ra_offset));
  let deallocate =
    if count mod 2 = 0
    then (
      fun ~now ->
        let () = now in
        emit ld ra (ROffset (SP, ra_offset));
        emit
          addi
          sp
          sp
          (8 * (count + 2))
          ~comm:
            (sprintf "DEallocate for Pad, RA and %d locals variables %s" count args_repr);
        List.iter Addr_of_local.remove_local local_names;
        Addr_of_local.last_pos := !Addr_of_local.last_pos - 2)
    else
      fun ~now ->
        let () = now in
        emit ld ra (ROffset (SP, ra_offset));
        emit
          addi
          sp
          sp
          (8 + (8 * count))
          ~comm:(sprintf "DEallocate for RA, and %d locals %s" count args_repr);
        List.iter Addr_of_local.remove_local local_names;
        Addr_of_local.last_pos := !Addr_of_local.last_pos - 1
  in
  deallocate, count
;;

(* TODO(Kakadu): remove? *)
let store_ra_temp f =
  let ra_temp_name = Ident.of_string @@ Printf.sprintf "temp_ra_%d" (gensym ()) in
  Addr_of_local.extend ra_temp_name;
  emit addi sp sp (-8) ~comm:(sprintf "alloc space for RA register");
  let rez = f ra_temp_name in
  emit ld ra (ROffset (SP, 0));
  (* printfn ppf "  ld ra, (sp)"; *)
  emit addi sp sp 8 ~comm:"free space of RA register";
  (* printfn ppf "  addi sp, sp, 8 # free space of RA register"; *)
  Addr_of_local.remove_local ra_temp_name;
  rez
;;

let with_two_slots f =
  let slot1 = Ident.of_string @@ Printf.sprintf "x%d" (gensym ()) in
  let slot2 = Ident.of_string @@ Printf.sprintf "x%d" (gensym ()) in
  Addr_of_local.extend slot1;
  Addr_of_local.extend slot2;
  (* TODO: +-16 on SP should go here  *)
  let rez = f slot1 slot2 in
  Addr_of_local.remove_local slot2;
  Addr_of_local.remove_local slot1;
  rez
;;

let print_epilogue ppf fname =
  if fname <> "main"
  then emit ret ~comm:fname
  else (
    emit comment " fin";
    emit addi (RU "a0") (RU "x0") 0 ~comm:"Use 0 return code";
    emit addi (RU "a7") (RU "x0") 93 ~comm:"Service command code 93 terminates";
    emit ecall ~comm:"Call linux to terminate the program");
  fprintf ppf "%!"
;;

let sd_dest k a = function
  | DReg s -> mv k a (RU s)
  | DStack_var name -> sd k a (Addr_of_local.pp_to_mach name)
;;

let li_dest k a n =
  match a with
  | DReg _ -> li k (Temp_reg 100500) n
  | DStack_var _ -> li k (Temp_reg 666) n
;;

let addi1dest k d b n =
  match d with
  | DReg name -> addi k (RU name) b n
  | DStack_var _ -> li k (Temp_reg __LINE__) n
;;

let pp_to_mach = Addr_of_local.pp_to_mach

let emit_alloc_closure fname arity =
  emit lla (RU "a0") fname;
  emit li (RU "a1") arity;
  emit call "rukaml_alloc_closure"
;;

(**
    Argument [is_toplevel] returns None or Some arity. *)
let generate_body is_toplevel body =
  let open Parsetree in
  let dealloc_locals, locals = allocate_locals body in
  let deallocate_args_for_call argc =
    let padded_argc : int = if argc mod 2 = 0 then argc else argc + 1 in
    Addr_of_local.(last_pos := !last_pos - padded_argc);
    emit addi SP SP (8 * padded_argc) ~comm:(sprintf "deallocate %d args" argc)
    (* printfn ppf "  addi sp, sp, 8*%d # deallocate %d args" argc argc *)
  in
  let allocate_args_for_call ?f args =
    (* Allocate args for a function call inside a body *)
    let count = List.length args in
    emit
      comment
      (sprintf "Allocate args to call fun %S with args" (Option.get f).Ident.hum_name);
    let stack_slots = if count mod 2 = 0 then count else 1 + count in
    Addr_of_local.last_pos := !Addr_of_local.last_pos + stack_slots;
    emit
      addi
      SP
      SP
      (-8 * stack_slots)
      ~comm:(sprintf "last_pos = %d" !Addr_of_local.last_pos);
    (* TODO(Kakadu): check RTL *)
    (* TODO(Kakadu): Rwrite to emit less code *)
    let pp_access ?(doc = "") v offset =
      emit li t0 v;
      emit sd t0 (ROffset (SP, 8 * offset)) ~comm:doc
    in
    let on_arg i arg =
      (* iteration is RTL *)
      match arg with
      | Compile_lib.ANF.AUnit | AConst (PConst_bool false) -> pp_access 0 i
      | AConst (PConst_bool true) -> pp_access 1 i
      | AConst (PConst_int n) -> pp_access ~doc:"constant" n i
      | AVar vname when Option.is_some (is_toplevel vname) ->
        (match is_toplevel vname with
         | Some arity ->
           emit_alloc_closure vname.hum_name arity;
           emit sd a0 (ROffset (SP, 8 * i))
         | None -> assert false)
      | AVar { Ident.hum_name = "print"; _ } ->
        emit_alloc_closure "rukaml_print_int" 1;
        (* Result is in a0 *)
        emit sd a0 (ROffset (SP, 8 * i))
      | AVar vname ->
        (* TODO: use pp_access *)
        emit ld t0 (pp_to_mach vname) ~comm:(sprintf "arg %S" vname.hum_name);
        emit sd t0 (ROffset (SP, 8 * i))
      | ALam _ -> failwith "Should it be representable in ANF?"
      | APrimitive _ -> assert false
      | ATuple _ -> assert false
      | AArray _ -> assert false
    in
    ListLabels.iteri args ~f:on_arg;
    (* printfn ppf "  addi sp, sp, -8*%d # fun %S arguments" count (Option.get f); *)
    count
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
    | CIte (CAtom (AConst (Parsetree.PConst_bool true)), bth, _bel) -> helper dest bth
    | CIte (CAtom (AConst (Parsetree.PConst_bool false)), _bth, bel) -> helper dest bel
    | CIte (CAtom (AVar econd), bth, bel) when Addr_of_local.contains econd ->
      (* if on global or local variable  *)
      emit ld t0 (pp_to_mach econd);
      (* printfn ppf "  ld  t0, %a" Addr_of_local.pp_local_exn econd; *)
      let el_lab = Printf.sprintf "lab_else_%d" (gensym ()) in
      let fin_lab = Printf.sprintf "lab_endif_%d" (gensym ()) in
      emit beq t0 zero el_lab;
      (* printfn ppf "  beq t0, zero, %s" el_lab; *)
      helper dest bth;
      emit beq zero zero fin_lab;
      (* printfn ppf "  beq zero, zero, %s" fin_lab; *)
      emit label el_lab ~comm:(sprintf "%s is 0" econd.hum_name);
      (* printfn ppf "%s:  # %s is 0 " el_lab econd; *)
      helper dest bel;
      emit label fin_lab
      (* printfn ppf "%s:" fin_lab *)
    | CIte
        ( CApp
            (APrimitive (("<" | "=" | "<=") as op), AVar vname, [ AConst (PConst_int n) ])
        , bthen
        , belse ) ->
      emit ld t0 (pp_to_mach vname) ~comm:(Format.asprintf "access %a" Ident.pp vname);
      emit li t1 n;
      let lab_then = Printf.sprintf "lab_then_%d" (gensym ()) in
      let lab_fin = Printf.sprintf "lab_fin_%d" (gensym ()) in
      let op_mnem =
        match op with
        | "<" -> blt
        | "=" -> beq
        | "<=" -> ble
        | _ -> failwith "Should not happen"
      in
      emit op_mnem t0 t1 lab_then;
      helper dest belse;
      emit beq zero zero lab_fin;
      emit label lab_then;
      helper dest bthen;
      emit label lab_fin
    | CApp (AVar f, arg1, [])
      when f.Ident.hum_name = "print"
           && is_toplevel f = None
           && not (Addr_of_local.has_key f) ->
      (match arg1 with
       | AVar v when Addr_of_local.has_key v ->
         with_two_slots (fun ra_name arg_name ->
           emit addi SP SP (-16);
           emit sd ra (pp_to_mach ra_name);
           emit ld t0 (pp_to_mach v);
           emit mv (pp_to_mach arg_name) t0;
           (* emit li a1 1;
                   emit li a2 2;
                   emit li a3 3;
                   emit li a4 4;
                   emit li a5 5;
                   emit li a6 6;
                   emit li a7 7; *)
           emit call "rukaml_print_int";
           emit ld ra (pp_to_mach ra_name);
           emit sd_dest a0 dest;
           emit addi SP SP 16)
       | AConst (PConst_int n) ->
         with_two_slots (fun ra_name arg_name ->
           emit addi SP SP (-16);
           emit li a0 n;
           emit sd a0 (pp_to_mach arg_name);
           emit sd ra (pp_to_mach ra_name);
           emit call "rukaml_print_int";
           emit ld ra (pp_to_mach ra_name);
           emit sd_dest a0 dest;
           emit addi SP SP 16)
       | AConst (PConst_bool _)
       | AArray _ | AVar _ | APrimitive _
       | ATuple (_, _, _)
       | ALam (_, _)
       | AUnit -> failwith "Should not happen")
    | CApp (APrimitive "=", AConst (PConst_int l), [ AConst (PConst_int r) ]) ->
      (* TODO: user Addr_of_local.pp_local_exn *)
      if l = r
      then (
        emit li t0 1;
        emit sd_dest t0 dest
        (* failwiths "not implemented %d" __LINE__ *)
        (* printfn ppf "  mov qword %a, 1" Addr_of_local.pp_dest dest *))
      else
        failwiths "not implemented %d" __LINE__
        (* printfn ppf "  mov qword %a, 0" Addr_of_local.pp_dest dest *)
    | CApp (APrimitive ("=" as op), AConst (PConst_int n), [ AVar vname ])
    | CApp (APrimitive (("=" | "<") as op), AVar vname, [ AConst (PConst_int n) ]) ->
      let branch_instr =
        match op with
        | "=" -> emit beq
        | "<" -> emit blt
        | _ -> assert false
      in
      let eq_lab = Printf.sprintf "lab_%d" (gensym ()) in
      let exit_lab = Printf.sprintf "lab_%d" (gensym ()) in
      emit
        comment
        (Format.asprintf
           "%a, find_exn %S = %d, last_pos = %d"
           Addr_of_local.pp_local_exn
           vname
           vname.hum_name
           (Addr_of_local.find_exn vname)
           !Addr_of_local.last_pos);
      emit ld t0 (pp_to_mach vname) ~comm:(sprintf "locals = %d" locals);
      (* printfn ppf "  ld t0, %a # locals = %d" Addr_of_local.pp_local_exn vname
           locals; *)
      emit li t1 n;
      (* printfn ppf "  li t1, %d" n; *)
      branch_instr t0 t1 eq_lab;
      emit sd_dest zero dest;
      (* printfn ppf "  sd zero, %a" Addr_of_local.pp_dest dest; *)
      emit beq zero zero exit_lab;
      (* printfn ppf "  beq zero, zero, %s # Where is unconditional jump?"
           exit_lab; *)
      emit label eq_lab;
      (* printfn ppf "%s:" eq_lab; *)
      emit li t0 1;
      (* printfn ppf "  li t0, 1 # (* RISC is weird *)"; *)
      emit sd_dest t0 dest ~comm:(Format.asprintf "dest = %a" Addr_of_local.pp_dest dest);
      (* printfn ppf "  sd t0, %a # dest = %a" Addr_of_local.pp_dest dest
           Addr_of_local.pp_dest dest; *)
      emit beq zero zero exit_lab;
      (* printfn ppf "  beq zero, zero, %s # not needed?" exit_lab; *)
      emit label exit_lab
      (* printfn ppf "%s:" exit_lab *)
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
         emit ld t0 (pp_to_mach vname);
         (* printfn ppf "  ld t0, %a" Addr_of_local.pp_local_exn vname; *)
         emit addi t0 t0 (-1);
         (* printfn ppf "  addi t0, t0, -1"; *)
         emit sd_dest t0 dest
         (* printfn ppf "  sd t0, %a" Addr_of_local.pp_dest dest *)
       | Some _ ->
         (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
         failwiths "not implemented %d" __LINE__)
    | CApp (APrimitive "-", AVar vname, [ AConst (PConst_int n) ]) ->
      (match is_toplevel vname with
       | None ->
         emit ld t5 (pp_to_mach vname);
         (* printfn ppf "  ld t5, %a #" Addr_of_local.pp_local_exn vname; *)
         emit addi t5 t5 (-n);
         (* printfn ppf "  addi t5, t5, -%d" n; *)
         emit sd_dest t5 dest
         (* printfn ppf "  sd t5, %a" Addr_of_local.pp_dest dest *)
       | Some _ ->
         (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
         failwiths "not implemented %d" __LINE__)
    | CApp (APrimitive (("+" | "*") as prim), AVar vname, [ AConst (PConst_int n) ])
    | CApp (APrimitive (("+" | "*") as prim), AConst (PConst_int n), [ AVar vname ]) ->
      (match is_toplevel vname with
       | None ->
         emit ld t0 (Addr_of_local.pp_to_mach vname);
         emit li t1 n;
         emit
           (match prim with
            | "+" -> add
            | "*" -> mulw
            | op -> failwiths "not implemented '%s' on %d" op __LINE__)
           t2
           t0
           t1;
         emit sd_dest t2 dest
       | Some _ ->
         (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
         failwiths "not implemented %d" __LINE__)
    | CApp (APrimitive (("+" | "*" | "-") as prim), AVar vl, [ AVar vr ]) ->
      emit comment (sprintf "%s is stored in %d" vl.hum_name (Addr_of_local.find_exn vl));
      emit comment (sprintf "%s is stored in %d" vr.hum_name (Addr_of_local.find_exn vr));
      emit comment (sprintf "last_pos = %d" !Addr_of_local.last_pos);
      emit ld t3 (Addr_of_local.pp_to_mach vl);
      (* printfn ppf "  ld t4, %a" Addr_of_local.pp_local_exn vr; *)
      emit ld t4 (Addr_of_local.pp_to_mach vr);
      (match prim with
       | "+" -> emit add
       | "*" -> emit mulw
       | "-" -> emit sub
       | op -> failwiths "not_implemeted  %S. %d" op __LINE__)
        t5
        t3
        t4;
      (match dest with
       | DReg _ ->
         emit addi1dest dest t5 0
         (* printfn ppf "  addi %a, t5, 0" Addr_of_local.pp_dest dest *)
       | DStack_var _ ->
         (* printfn ppf "  sd t5, %a" Addr_of_local.pp_dest dest *)
         emit sd_dest t5 dest)
    | CApp (AVar f, arg1, args) when Option.is_some (is_toplevel f) ->
      (* Callig a rukaml function uses custom calling convention.
           Pascal convention: all arguments on stack, LTR *)
      let expected_arity = Option.get (is_toplevel f) in
      let formal_arity = 1 + List.length args in
      (* printfn ppf "\t; expected_arity = %d\n\t; formal_arity = %d"
             expected_arity formal_arity;
           printfn ppf "\t; calling %S" f; *)
      if expected_arity = formal_arity
      then (
        let _ =
          let to_remove = allocate_args_for_call ~f (arg1 :: args) in
          emit call f.hum_name;
          deallocate_args_for_call to_remove;
          to_remove
        in
        emit sd_dest (RU "a0") dest)
      else if formal_arity < expected_arity
      then
        with_two_slots (fun ra_name func_clo_id ->
          emit addi sp sp (-16) ~comm:(sprintf " RA + closure");
          emit sd ra (Addr_of_local.pp_to_mach ra_name);
          emit lla a0 f.hum_name;
          emit li a1 expected_arity;
          emit call "rukaml_alloc_closure";
          emit sd a0 (Addr_of_local.pp_to_mach func_clo_id);
          let _partial_args_count =
            allocate_args_for_call ~f (arg1 :: args)
            (* Needed because we allocate temporary space to prepare arguments  *)
          in
          let () =
            emit ld a0 (Addr_of_local.pp_to_mach func_clo_id);
            emit li a1 formal_arity;
            assert (formal_arity < 5);
            (* See calling convention *)
            List.iteri
              (fun i rname ->
                 emit ~comm:(sprintf "arg %d" i) ld (RU rname) (ROffset (SP, 8 * i)))
              (list_take formal_arity [ (*"a0"; *) "a2"; "a3"; "a4"; "a5" ])
          in
          emit call "rukaml_applyN";
          emit sd_dest (RU "a0") dest;
          deallocate_args_for_call formal_arity;
          emit ld ra (Addr_of_local.pp_to_mach ra_name);
          emit addi sp sp 16 ~comm:"deallocate RA + closure")
      else failwith "Arity mismatch: over application"
    | CApp (AVar f, (AConst _ as arg), []) | CApp (AVar f, (AVar _ as arg), []) ->
      (* A 1 argument application *)
      assert (Option.is_none (is_toplevel f));
      with_two_slots (fun _ arg1 ->
        emit addi SP SP (-16) ~comm:(sprintf "pad and 1st arg of function %s" f.hum_name);
        helper_a (DStack_var arg1) arg;
        emit ld (RU "a0") (Addr_of_local.pp_to_mach f);
        emit li (RU "a1") 1;
        emit ld (RU "a2") (Addr_of_local.pp_to_mach arg1);
        emit call "rukaml_applyN";
        emit
          addi
          SP
          SP
          16
          ~comm:(sprintf "DEalloc for pad and arg 1 of function %S" f.hum_name));
      if dest <> DReg "a0" then emit sd_dest (RU "a0") dest
    | CApp (APrimitive "field", AConst (PConst_int _n), [ AVar _ ]) ->
      failwiths "Not implemented"
      (* helper_a (DReg "rsi") cont;
           printfn ppf "  mov rdi, %d" n;
           printfn ppf "  call rukaml_field";
           printfn ppf "  mov %a, rax" Addr_of_local.pp_dest dest *)
    | CApp (AVar id, AUnit, []) when id.hum_name = "gc_compact" ->
      failwiths "Not implemented"
      (* printfn ppf "  mov rdi, rsp";
           printfn ppf "  mov rsi, 0";
           printfn ppf "  call rukaml_gc_compact" *)
    | CApp (AVar id, AUnit, []) when id.hum_name = "gc_stats" ->
      failwiths "Not implemented %s %d" __FILE__ __LINE__
      (* printfn ppf "  mov rdi, 0";
           printfn ppf "  mov rsi, 0";
           printfn ppf "  call rukaml_gc_print_stats" *)
    | CAtom atom -> helper_a dest atom
    | CApp _ as anf ->
      Format.eprintf "Unsupported: @[`%a`@]\n%!" Compile_lib.ANF.pp_c anf;
      failwiths "Not implemented %d" __LINE__
    | _rest -> emit comment (sprintf ";;; TODO %s %d" __FUNCTION__ __LINE__)
  (* printfn ppf "; @[<h>%a@]" Compile_lib.ANF.pp_c rest *)
  and helper_a (dest : dest) x =
    (* log "  %s: dest=`%a`, expr = %a" __FUNCTION__ pp_dest dest ANF.pp_a x; *)
    match x with
    | AConst (Parsetree.PConst_bool true) ->
      emit li_dest dest 1 (* printfn ppf "  li %a, 1" Addr_of_local.pp_dest dest *)
    | AConst (Parsetree.PConst_int n) ->
      (match dest with
       | DReg r -> emit li (RU r) n
       (* printfn ppf "  li %a, %d" Addr_of_local.pp_dest dest n *)
       | DStack_var _ ->
         emit li t0 n;
         (* printfn ppf "  li t0, %d" n; *)
         emit sd_dest t0 dest
         (* printfn ppf "  sd t0, %a" Addr_of_local.pp_dest dest *))
    | AVar vname ->
      (match is_toplevel vname with
       | None ->
         emit ld t5 (Addr_of_local.pp_to_mach vname);
         (* printfn ppf "  ld t5, %a" Addr_of_local.pp_local_exn vname; *)
         (match dest with
          | DReg _ ->
            emit addi1dest dest t5 0
            (* printfn ppf "  addi %a, t5, 0 # 24" Addr_of_local.pp_dest dest *)
          | DStack_var _ ->
            emit sd_dest t5 dest ~comm:(sprintf "access a var %S" vname.hum_name))
         (* printfn ppf "  sd t5, %a # access a var %S"
               Addr_of_local.pp_dest dest vname *)
       | Some arity ->
         emit_alloc_closure vname.hum_name arity;
         (* print_alloc_closure ppf vname arity; *)
         emit sd_dest (RU "a0") dest
         (* printfn ppf "  sd a0, %a" Addr_of_local.pp_dest dest *))
    | ATuple (_a, _b, []) ->
      failwiths "not implemented %s %d" __FILE__ __LINE__
      (* store_ra_temp ppf (fun ra_name ->
            helper_a (DReg "r0") a;
            helper_a (DReg "r1") b;

            printfn ppf "  sd ra, %a" Addr_of_local.pp_local_exn ra_name;
            printfn ppf "  call rukaml_alloc_pair";
            printfn ppf "  mv %a, a0" Addr_of_local.pp_dest dest) *)
    | AUnit ->
      failwiths "not implemented %s %d" __FILE__ __LINE__
      (* printfn ppf "mov qword %a, 0" Addr_of_local.pp_dest dest *)
    | _atom ->
      (* printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__; *)
      failwiths "not implemented %s %d" __FILE__ __LINE__
    (* printfn ppf ";;; @[`%a`@]" ANF.pp_a atom *)
  in
  helper (DReg "a0") body;
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
  (* log "ANF: @[%a@]" Compile_lib.ANF.pp_stru anf; *)
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
    (* printfn ppf "section .note.GNU-stack noalloc noexec nowrite progbits"; *)
    if use_custom_main
    then
      printfn
        ppf
        {|section .data
            newline_char: db 10
            codes: db '0123456789abcdef' |};
    (* printfn ppf "section .text"; *)
    if use_custom_main
    then (
      put_print_newline ppf;
      put_print_hex ppf);
    (* externs *)
    let __ () =
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
    if wrap_main_into_start
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
    let on_vb (_flg, name, expr) =
      if Addr_of_local.size () <> 0
      then
        failwiths
          "There are left over variables (before function %s): %s "
          name.Ident.hum_name
          (Addr_of_local.keys ());

      (* printfn ppf "";
           fprintf ppf "\t; %a\n" Loc_of_ident.pp (); *)

      (* print_prologue ppf name; *)

      (* fprintf ppf "  ; There are %d known arguments in %s\n%!"
           (Loc_of_ident.size ()) name; *)

      (* printfn ppf "  sub rsp, %d" (8 * Loc_of_ident.size ()); *)
      let () = printfn ppf "\n.globl %s" name.Ident.hum_name in
      let () = printfn ppf "%s:" name.Ident.hum_name in
      let pats, body = ANF.group_abstractions expr in
      let argc = List.length pats in
      let names =
        List.map
          (function
            | ANF.APname name -> name)
          pats
      in
      let _ = if argc mod 2 = 0 then argc else argc + 1 in
      let () =
        if name.Ident.hum_name = "main"
        then emit comment "this is main"
        else
          List.rev pats
          |> ListLabels.iteri ~f:(fun i -> function
            | ANF.APname name -> Addr_of_local.add_arg ~argc i name)
      in
      generate_body is_toplevel body;
      Addr_of_local.remove_args names;
      print_epilogue ppf name.hum_name;
      Machine.flush_queue ppf
    in
    List.iter on_vb anf;
    Format.pp_print_flush ppf ());
  Result.Ok ()
;;
