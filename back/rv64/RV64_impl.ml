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

module String_lit_hash = struct
  include Hashtbl.Make (struct
      include String

      let hash = Hashtbl.hash
    end)

  let last = ref 0

  let extend key hash =
    if mem hash key
    then ()
    else (
      incr last;
      add hash key !last)
  ;;
end

let string_list_hash = String_lit_hash.create 42
let iter_string_lit_hash f = String_lit_hash.iter f string_list_hash

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
    (* log "Location argument \"%a\" in [rbp+%d]" Ident.pp name (-loc); *)
    Hashtbl.add store name loc
  ;;

  let remove_args xs =
    (* log "Removing info about args [ %s ]" (Ident.concat_str xs); *)
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
    | ELet (_flg, Tpat_unit, rhs, where_) ->
      helper_c rhs;
      helper where_
    | ELet _ as anf -> failwiths "Not implemented: @[%a@]" ANF.pp anf
  and helper_c = function
    | CIte (_, th, el) ->
      helper th;
      helper el
    | CTuple _ -> ()
    | CString_const _ | CApp _ | CAtom _ -> ()
  in
  helper input_anf;
  let local_names = Ident.Ident_set.to_list !local_names in
  let count = List.length local_names in
  let args_repr = Ident.concat_str local_names in
  let ra_offset =
    let sp_offset = if count mod 2 = 0 then count + 2 else count + 1 in
    emit addi sp sp (-8 * sp_offset);
    Addr_of_local.last_pos := !Addr_of_local.last_pos + sp_offset - count;
    ListLabels.iter (List.rev local_names) ~f:(fun name -> Addr_of_local.extend name);
    ListLabels.iter local_names ~f:(fun name ->
      let comm = Format.asprintf "loc for %a" Ident.pp name in
      emit sd zero (Addr_of_local.pp_to_mach name) ~comm);
    emit sd ra (ROffset (SP, 8 * count));
    if count mod 2 = 0
    then (
      let comm = "padding" in
      emit sd zero (ROffset (SP, (8 * count) + 8)) ~comm);
    8 * count
  in
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
  | DReg s -> mv k (RU s) a
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

type def_kind =
  [ `Func of int (** normal toplelvel function *)
  | `Val (* a toplevel value in special ELF block *)
  | `External (* Unknown or standard function *)
  | `Local
  ]

let pp_def_kind ppf = function
  | `Func n -> Format.fprintf ppf "Func %d" n
  | `Val -> Format.fprintf ppf "Val"
  | `External -> Format.fprintf ppf "External"
  | `Local -> Format.fprintf ppf "`Local"
;;

(**
    Argument [is_toplevel] returns None or Some arity. *)
let generate_body is_toplevel body =
  let is_toplevel_func id =
    match is_toplevel id with
    | `Func _ -> true
    | _ -> false
  in
  let _ : Ident.t -> def_kind = is_toplevel in
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
    let comm = sprintf "last_pos = %d" !Addr_of_local.last_pos in
    emit addi SP SP (-8 * stack_slots) ~comm;
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
      | AConst (PConst_char c) -> pp_access ~doc:"constant" (Char.code c) i
      | AConst (PConst_string _) ->
        log
          "%s %a"
          (match f with
           | None -> "?"
           | Some id -> Ident.to_string id)
          (pp_space_list ANF.pp_a)
          args;
        failwith "TODO: not implemented"
      | AVar vname when is_toplevel_func vname ->
        (match is_toplevel vname with
         | `Func arity ->
           emit_alloc_closure vname.hum_name arity;
           emit sd a0 (ROffset (SP, 8 * i))
         | _ -> assert false)
      | AVar { Ident.hum_name = "char_code"; _ } ->
        emit_alloc_closure "rukaml_identity" 1;
        (* Result is in a0 *)
        emit sd a0 (ROffset (SP, 8 * i))
      | AVar { Ident.hum_name = "array_len"; _ } ->
        emit_alloc_closure "rukaml_array_length" 1;
        (* Result is in a0 *)
        emit sd a0 (ROffset (SP, 8 * i))
      | AVar { Ident.hum_name = "array_get"; _ } ->
        emit_alloc_closure "rukaml_array_get" 2;
        (* Result is in a0 *)
        emit sd a0 (ROffset (SP, 8 * i))
      | AVar { Ident.hum_name = "array_set"; _ } ->
        emit_alloc_closure "rukaml_array_set" 3;
        (* Result is in a0 *)
        emit sd a0 (ROffset (SP, 8 * i))
      | AVar { Ident.hum_name = "stdin"; _ } -> emit call "rukaml_array_stdin"
      (* Result is in a0 *)
      | AVar vname ->
        (* TODO: use pp_access *)
        emit ld t0 (pp_to_mach vname) ~comm:(sprintf "arg %S" vname.hum_name);
        emit sd t0 (ROffset (SP, 8 * i))
      | ALam _ -> failwith "Should it be representable in ANF?"
      | APrimitive ("print", (1 as parity)) ->
        emit_alloc_closure "rukaml_print_int_kaml" parity;
        emit sd a0 (ROffset (SP, 8 * i))
      | APrimitive ("stdout", 0) -> emit li a0 1
      | APrimitive _ as arg -> failwiths "Primitive %a is not supported" ANF.pp_a arg
      | AArray _ -> failwiths "Can't handle argument '%a'" ANF.pp_a arg
      | AConstruct (tag, []) -> pp_access tag i
      | AConstruct _ -> failwiths "Can't handle argument '%a'" ANF.pp_a arg
    in
    ListLabels.iteri args ~f:on_arg;
    (* printfn ppf "  addi sp, sp, -8*%d # fun %S arguments" count (Option.get f); *)
    count
  in
  let rec helper dest x =
    match x with
    | Compile_lib.ANF.EComplex c -> helper_c dest c
    | ELet (_, Tpat_var name, rhs, wher) ->
      assert (Addr_of_local.contains name);
      let local = DStack_var name in
      helper_c local rhs;
      helper dest wher
    | ELet (_, Tpat_unit, rhs, wher) ->
      helper_c (DReg "zero") rhs;
      helper dest wher
    | ELet _ as anf -> failwiths "Not implemented: @[%a@]" ANF.pp anf
  and helper_c (dest : dest) x =
    match x with
    | CIte (CAtom (AConst (Parsetree.PConst_bool true)), bth, _bel) -> helper dest bth
    | CIte (CAtom (AConst (Parsetree.PConst_bool false)), _bth, bel) -> helper dest bel
    | CIte
        ( CApp
            ( APrimitive ("=", 2)
            , AConst (Parsetree.PConst_int l)
            , [ AConst (Parsetree.PConst_int r) ] )
        , bth
        , bel ) ->
      (* This is not entirely correct, because OCaml number and target could be different *)
      helper dest (if l = r then bth else bel)
    | CIte (CAtom (ANF.AConst (PConst_int 1)), bth, _) -> helper dest bth
    | CIte (CAtom (AVar econd), bth, bel) when Addr_of_local.contains econd ->
      (* if on global or local variable  *)
      emit ld t0 (pp_to_mach econd);
      let el_lab = Printf.sprintf "lab_else_%d" (gensym ()) in
      let fin_lab = Printf.sprintf "lab_endif_%d" (gensym ()) in
      emit beq t0 zero el_lab;
      helper dest bth;
      emit beq zero zero fin_lab;
      emit label el_lab ~comm:(sprintf "%s is 0" econd.hum_name);
      helper dest bel;
      emit label fin_lab
    | CIte
        ( CApp
            ( APrimitive ((("<" | "=" | "<=") as op), _)
            , ((AVar _ | AConst (PConst_int _)) as lop)
            , [ ((AVar _ | AConst (PConst_int _)) as rop) ] )
        , bthen
        , belse ) ->
      let prepare_operand dest = function
        | ANF.AVar vname ->
          let comm = Format.asprintf "access %a" Ident.pp vname in
          emit ld dest (pp_to_mach vname) ~comm
        | AConst (PConst_int n) -> emit li dest n
        | _ -> assert false
      in
      prepare_operand t0 lop;
      prepare_operand t1 rop;
      (* emit ld t0 (pp_to_mach vname) ~comm:(Format.asprintf "access %a" Ident.pp vname);
      emit li t1 n; *)
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
      when f.Ident.hum_name = "char_code"
           && is_toplevel f = `External
           && not (Addr_of_local.has_key f) ->
      (match arg1 with
       | AVar v when Addr_of_local.has_key v ->
         emit ld t0 (pp_to_mach v);
         emit sd_dest t0 dest
       | AConst (PConst_char c) ->
         emit li t0 (Char.code c);
         emit sd_dest t0 dest
       | _ -> failwith "Should not happen: char_code")
    | CAtom (AVar f)
      when f.Ident.hum_name = "stdin"
           && is_toplevel f = `External
           && not (Addr_of_local.has_key f) ->
      emit call "rukaml_array_stdin";
      emit sd_dest a0 dest
    | CApp (AVar f, arg1, [])
      when f.Ident.hum_name = "open_in"
           && is_toplevel f = `External
           && not (Addr_of_local.has_key f) ->
      (match arg1 with
       | AVar v when Addr_of_local.has_key v ->
         with_two_slots (fun ra_name arg_name ->
           emit addi SP SP (-16);
           emit sd ra (pp_to_mach ra_name);
           emit ld t0 (pp_to_mach v);
           emit mv (pp_to_mach arg_name) t0;
           emit call "rukaml_array_read_in";
           emit ld ra (pp_to_mach ra_name);
           emit sd_dest a0 dest;
           emit addi SP SP 16)
       | AArray _ as r ->
         with_two_slots (fun ra_name arg_name ->
           emit addi SP SP (-16);
           emit sd ra (pp_to_mach ra_name);
           emit sd a0 (pp_to_mach arg_name);
           helper_a (DReg "a0") r;
           emit call "rukaml_array_read_in";
           emit ld ra (pp_to_mach ra_name);
           emit sd_dest a0 dest;
           emit addi SP SP 16)
       | _ -> failwith "Should not happen: open_in")
    | CApp (APrimitive ("print", 1), arg1, []) ->
      (match arg1 with
       | AVar v when Addr_of_local.has_key v ->
         emit ld a0 (pp_to_mach v);
         emit call "rukaml_print_int";
         emit sd_dest zero dest
       | AConst (PConst_int n) ->
         emit li a0 n;
         emit call "rukaml_print_int";
         emit sd_dest zero dest
       | AConst _ | AArray _ | AVar _ | APrimitive _ | AConstruct _ | ALam (_, _) | AUnit
         -> failwith "Should not happen: print_int")
    | CApp (AVar f, arg1, [])
      when f.Ident.hum_name = "array_len"
           && is_toplevel f = `External
           && not (Addr_of_local.has_key f) ->
      (match arg1 with
       | AVar v when Addr_of_local.has_key v ->
         with_two_slots (fun ra_name arg_name ->
           emit addi SP SP (-16);
           emit sd ra (pp_to_mach ra_name);
           emit ld t0 (pp_to_mach v);
           emit mv (pp_to_mach arg_name) t0;
           emit call "rukaml_array_length";
           emit ld ra (pp_to_mach ra_name);
           emit sd_dest a0 dest;
           emit addi SP SP 16)
       | AArray _ | _ -> failwith "Should not happen")
    | CApp (AVar f, arg1, [])
      when f.Ident.hum_name = "array_get"
           && is_toplevel f = `External
           && not (Addr_of_local.has_key f) ->
      (match arg1 with
       | AVar arr when Addr_of_local.has_key arr ->
         emit_alloc_closure "rukaml_array_get" 2;
         emit li a1 1;
         emit ld a2 (pp_to_mach arr);
         emit call "rukaml_applyN";
         emit sd_dest a0 dest
       | _ -> failwith "Should not happen")
    | CApp (AVar f, arg1, [])
      when f.Ident.hum_name = "array_set"
           && is_toplevel f = `External
           && not (Addr_of_local.has_key f) ->
      (match arg1 with
       | AVar arr when Addr_of_local.has_key arr ->
         emit_alloc_closure "rukaml_array_set" 3;
         emit li a1 1;
         emit ld a2 (pp_to_mach arr);
         emit call "rukaml_applyN";
         emit sd_dest a0 dest
       | _ -> failwith "Should not happen")
    | CApp (APrimitive ("=", _), AConst (PConst_int l), [ AConst (PConst_int r) ]) ->
      (* TODO: user Addr_of_local.pp_local_exn *)
      if l = r
      then (
        emit li t0 1;
        emit sd_dest t0 dest)
      else
        failwiths "not implemented %d" __LINE__
        (* printfn ppf "  mov qword %a, 0" Addr_of_local.pp_dest dest *)
    | CApp (APrimitive ("&&", _), al, [ ar ]) ->
      let on_arg dest = function
        | ANF.AVar vname -> emit ld dest (pp_to_mach vname)
        | _ -> failwiths "Not implemented: %s %d" __FILE__ __LINE__
      in
      on_arg t0 al;
      on_arg t1 ar;
      emit and_ t0 t0 t1;
      emit sd_dest t0 dest
    | CApp (APrimitive (("=" as op), _), AConst (PConst_int n), [ AVar vname ])
    | CApp (APrimitive ((("=" | "<") as op), _), AVar vname, [ AConst (PConst_int n) ]) ->
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
      emit li t1 n;
      branch_instr t0 t1 eq_lab;
      emit sd_dest zero dest;
      emit beq zero zero exit_lab;
      emit label eq_lab;
      emit li t0 1;
      emit sd_dest t0 dest ~comm:(Format.asprintf "dest = %a" Addr_of_local.pp_dest dest);
      emit beq zero zero exit_lab;
      emit label exit_lab
    | CApp (APrimitive ("-", 2), AVar vname, [ AConst (PConst_int n) ]) ->
      (match is_toplevel vname with
       | `External ->
         emit ld t5 (pp_to_mach vname);
         emit addi t5 t5 (-n);
         emit sd_dest t5 dest
       | `Local ->
         emit ld t5 (pp_to_mach vname);
         emit addi t5 t5 (-n);
         emit sd_dest t5 dest
       | _dk ->
         (* Format.eprintf "dk : %a\n%!" pp_def_kind dk; *)
         (* Format.eprintf "complex: @[%a@]\n%!" ANF.pp_c complex; *)
         (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
         failwiths "not implemented %d" __LINE__)
    | CApp (APrimitive ((("+" | "*") as prim), _), AVar vname, [ AConst (PConst_int n) ])
    | CApp (APrimitive ((("+" | "*") as prim), _), AConst (PConst_int n), [ AVar vname ])
      ->
      (match is_toplevel vname with
       | `Local ->
         emit ld t0 (Addr_of_local.pp_to_mach vname);
         emit li t1 n;
         emit comment "going to do some arithmetic";
         emit
           (match prim with
            | "+" -> add
            | "*" -> mulw
            | op -> failwiths "not implemented '%s' on %d" op __LINE__)
           t2
           t0
           t1;
         emit sd_dest t2 dest
       | dk ->
         (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
         log "def_kind=%a" pp_def_kind dk;
         failwiths "not implemented %d" __LINE__)
    | CApp (APrimitive (">=", info), vl, [ vr ]) ->
      (* This could be buggy *)
      helper_c dest (ANF.CApp (ANF.APrimitive ("<=", info), vr, [ vl ]))
    | CApp (APrimitive ("<=", _), AVar vl, [ AVar vr ]) ->
      emit ld t0 (Addr_of_local.pp_to_mach vl);
      emit ld t1 (Addr_of_local.pp_to_mach vr);
      emit addi t1 t1 1;
      emit slt t2 t0 t1;
      emit sd_dest t2 dest
    | CApp (APrimitive ((("+" | "*" | "-") as prim), _), AVar vl, [ AVar vr ]) ->
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
      emit sd_dest t5 dest
    | CApp (AVar f, arg, []) when f.Ident.hum_name = "trace_rukaml_val" ->
      (* assert (`External = is_toplevel f); *)
      helper_a (DReg "a0") arg;
      emit li (RU "a1") 0;
      emit call "rukaml_trace_val";
      if dest <> DReg "a0" then emit sd_dest (RU "a0") dest
    | CApp (AVar f, arg1, args) when is_toplevel_func f ->
      (* Calling a rukaml function uses custom calling convention.
           Pascal convention: all arguments on stack, LTR *)
      let expected_arity =
        match is_toplevel f with
        | `Func x -> x
        | _ -> assert false
      in
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
    | (CApp (AVar f, (AConst _ as arg), []) | CApp (AVar f, (AVar _ as arg), [])) as cexpr
      ->
      (* A 1 argument application *)

      (* log "cexpr = @[%a@]" ANF.pp_c cexpr; *)
      with_two_slots (fun arg0 arg1 ->
        emit addi SP SP (-16) ~comm:(sprintf "pad and 1st arg of function %s" f.hum_name);
        let floc =
          match f.Ident.hum_name, is_toplevel f with
          | "fprintf", `External ->
            emit call "rukaml_alloc_fprintf_closure";
            emit sd a0 (Addr_of_local.pp_to_mach arg0);
            fun () -> emit ld a0 (Addr_of_local.pp_to_mach arg0)
          | _, `Local -> fun () -> emit ld (RU "a0") (Addr_of_local.pp_to_mach f)
        in
        helper_a (DStack_var arg1) arg;
        floc ();
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
    | CApp
        ( APrimitive (("get_arg" | "field" | "block_nth"), _)
        , AVar arg
        , [ AConst (PConst_int idx) ] )
      when Addr_of_local.has_key arg ->
      emit li a0 idx;
      emit ld a1 (pp_to_mach arg);
      emit call "rukaml_field";
      emit sd_dest a0 dest
    | CApp
        ( APrimitive (("get_arg" | "field" | "block_nth"), _)
        , AVar _from
        , [ AConst (PConst_int _idx) ] ) ->
      failwiths "Not implemented: %s %d" __FILE__ __LINE__
    | CApp (AVar id, AUnit, []) when id.hum_name = "gc_compact" ->
      failwiths "Not implemented: %s %d" __FILE__ __LINE__
    | CApp (AVar id, AUnit, []) when id.hum_name = "gc_stats" ->
      failwiths "Not implemented %s %d" __FILE__ __LINE__
    | CApp (APrimitive ("output_string", 2), AVar ch, [ AVar arg ]) ->
      emit ld a0 (pp_to_mach ch);
      emit ld a1 (pp_to_mach arg);
      emit call "rukaml_output_string_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("output_char", 2), APrimitive ("stdout", 0), [ arg ]) ->
      let on_arg dest = function
        | ANF.AVar arg1 -> emit ld dest (pp_to_mach arg1)
        | AConst (PConst_int n) -> emit li dest n
        | _ -> assert false
      in
      emit li a0 1;
      on_arg a1 arg;
      emit call "rukaml_output_char_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("output_string", 2), APrimitive ("stdout", 0), [ AVar arg ]) ->
      emit li a0 1;
      emit ld a1 (pp_to_mach arg);
      emit call "rukaml_output_string_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("output_int", 2), APrimitive ("stdout", 0), [ AVar arg ]) ->
      emit li a0 1;
      emit ld a1 (pp_to_mach arg);
      emit call "rukaml_output_int_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("print_newline", 1), _arg_unit, []) ->
      emit li a0 0;
      emit call "rukaml_print_newline_sysv";
      emit sd_dest zero dest
    | CApp (APrimitive ("string_len", 1), AVar arg, []) ->
      emit ld a0 (pp_to_mach arg);
      emit call "rukaml_string_len_imm";
      (* TODO: Why imm?*)
      emit sd_dest a0 dest
    | CApp (APrimitive ("char_code", 1), AVar arg, []) ->
      emit ld t0 (pp_to_mach arg);
      emit sd_dest t0 dest
    | CApp (APrimitive ("fprintf", 1), AVar arg, []) ->
      emit ld a0 (pp_to_mach arg);
      emit call "rukaml_alloc_fprintf_closure";
      emit sd_dest a0 dest
    | CApp (APrimitive ("block_tag", _), AVar arg, []) ->
      emit ld a0 (pp_to_mach arg);
      emit call "rukaml_tag0";
      emit sd_dest a0 dest ~comm:(Format.asprintf "got tag of '%a'" Ident.pp arg)
    (* | CApp (APrimitive ("block_nth", _), AVar from, [ AConst (PConst_int idx) ]) *)
    | CApp (APrimitive ("get_arg", _), AConst (PConst_int idx), [ AVar from ])
      when Addr_of_local.has_key from ->
      emit li a0 idx;
      emit ld a1 (pp_to_mach from);
      emit call "rukaml_field"
    | CApp (APrimitive ("char_code", _), AConst (PConst_int c), []) ->
      emit li t0 c;
      emit sd_dest a0 dest
    | CApp (APrimitive ("char_code", _), AVar v, []) ->
      emit ld a0 (pp_to_mach v);
      emit call "char_code";
      emit sd_dest a0 dest
    | CApp (APrimitive ("match_failure", _), AConst (PConst_int c), []) ->
      emit li a0 c;
      emit call "rukaml_match_failure";
      emit sd_dest a0 dest
    | CApp (APrimitive ("string_of_char_list", 1), (AConstruct (0, []) as arg), []) ->
      helper_a (DReg "t5") arg;
      emit mv a0 t5;
      emit call "rukaml_string_of_char_list_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("string_of_char_list", 1), AVar v, []) ->
      emit ld a0 (pp_to_mach v);
      emit call "rukaml_string_of_char_list_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("string_nth", parity), arg1, [ arg2 ]) ->
      assert (parity = 2);
      let on_arg dest = function
        | ANF.AVar arg1 -> emit ld dest (pp_to_mach arg1)
        | AConst (PConst_int n) -> emit li dest n
        | _ -> assert false
      in
      on_arg a0 arg1;
      on_arg a1 arg2;
      emit call "rukaml_string_nth_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("string_of_int", 1), AVar v, []) ->
      emit ld a0 (pp_to_mach v);
      emit call "rukaml_string_of_int_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("string_of_int", 1), AConst (PConst_int n), []) ->
      emit li a0 n;
      emit call "rukaml_string_of_int_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("||", 2), AVar arg1, [ AVar arg2 ]) ->
      emit ld t1 (pp_to_mach arg1);
      emit ld t2 (pp_to_mach arg2);
      emit or_ t0 t1 t2;
      emit sd_dest t0 dest
    | CApp (APrimitive ("<", 2), AConst (PConst_int arg1), [ AVar arg2 ]) ->
      emit li t1 arg1;
      emit ld t2 (pp_to_mach arg2);
      emit slt t0 t1 t2;
      emit sd_dest t0 dest
    | CApp (APrimitive ("array_get", 2), AVar arg1, [ AConst (PConst_int arg2) ]) ->
      emit ld a0 (pp_to_mach arg1);
      emit li a1 arg2;
      emit call "rukaml_array_get_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("array_set", 3), AVar arg0, [ arg1; arg2 ]) ->
      let on_arg dest = function
        | ANF.AVar v -> emit ld dest (pp_to_mach v)
        | AConst (PConst_int n) -> emit li dest n
        | a ->
          Format.eprintf "arg = %a\n%!" ANF.pp_a a;
          failwiths "Not implemented. %s %d" __FILE__ __LINE__
      in
      emit ld a0 (pp_to_mach arg0);
      on_arg a1 arg1;
      on_arg a2 arg2;
      emit call "rukaml_array_set_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("printf", 1), AVar arg0, []) ->
      emit ld a0 (pp_to_mach arg0);
      emit call "rukaml_alloc_printf_closure0";
      emit sd_dest a0 dest
    | CApp (APrimitive ("fprintf", 2), APrimitive ("stdout", 0), [ AVar arg1 ]) ->
      emit li a0 1;
      emit ld a1 (pp_to_mach arg1);
      emit call "rukaml_alloc_fprintf_closure0";
      emit sd_dest a0 dest
    | CApp (APrimitive ("fprintf", 2), AVar arg0, [ AVar arg1 ]) ->
      emit ld a0 (pp_to_mach arg0);
      emit ld a1 (pp_to_mach arg1);
      emit call "rukaml_alloc_fprintf_closure0";
      emit sd_dest a0 dest
    | CApp (APrimitive (pname, partiy), arg1, args) ->
      Format.eprintf "At %s:%d\n%!" __FILE__ __LINE__;
      Format.eprintf "Unsupported primitive call: %s/%d\n%!" pname partiy;
      Format.eprintf "args: @[ %a @]\n%!" (pp_space_list ANF.pp_a) (arg1 :: args);
      failwiths "Not implemented %d" __LINE__
    | CApp _ as anf ->
      Format.eprintf "Unsupported: @[`%a`@]\n%!" Compile_lib.ANF.pp_c anf;
      failwiths "Not implemented %d" __LINE__
    | CAtom atom ->
      emit
        comment
        (Format.asprintf "  %s %d. atom = @[%a@]" __FUNCTION__ __LINE__ ANF.pp_a atom);
      helper_a dest atom
    | CString_const s ->
      let rukaml_val_loc n = sprintf "my_STRING_LIT_%d" n in
      emit lla t0 (rukaml_val_loc (String_lit_hash.find string_list_hash s));
      emit ld t0 (ROffset (Temp_reg 0, 0));
      emit sd_dest t0 dest
    | CTuple (x1, x2, xs) ->
      emit_initialize_block dest ~fields:(x1 :: x2 :: xs) ~tag:0 ~name:"tuple"
    | _rest ->
      Format.eprintf "@[%a@]\n%!" Compile_lib.ANF.pp_c _rest;
      failwiths "Not implemented %s %d" __FILE__ __LINE__
  and helper_a (dest : dest) x =
    log "  %s: expr = %a" __FUNCTION__ ANF.pp_a x;
    match x with
    | AConst (Parsetree.PConst_int n) ->
      (match dest with
       | DReg r ->
         (* TODO(Kakadu): LI works only for small numbers *)
         emit li (RU r) n
       | DStack_var _ ->
         emit li t0 n;
         emit sd_dest t0 dest)
    | AConst (Parsetree.PConst_char c) ->
      let n = Char.code c in
      (match dest with
       | DReg r -> emit li (RU r) n
       | DStack_var _ ->
         emit li t0 n;
         emit sd_dest t0 dest)
    | AVar vname ->
      (match is_toplevel vname with
       | `External -> failwiths "Only primitives could be external"
       | `Local ->
         emit ld t0 (Addr_of_local.pp_to_mach vname);
         (match dest with
          | DReg _ -> emit addi1dest dest t0 0
          | DStack_var _ ->
            emit sd_dest t0 dest ~comm:(sprintf "access a var %S" vname.hum_name))
       | `Func arity ->
         emit_alloc_closure vname.hum_name arity;
         emit sd_dest a0 dest
       | `Val ->
         Format.eprintf "helper_a. x = %a\n%!" ANF.pp_a x;
         failwiths "Not implemented: %s %d" __FILE__ __LINE__)
    | AArray r ->
      emit li a0 (List.length r);
      emit call "rukaml_alloc_array";
      List.iteri
        (fun i x ->
           helper_a (DReg "t0") x;
           emit sd t0 (ROffset (a0, 8 * i)))
        (List.rev r);
      emit sd_dest a0 dest
    | AConstruct (tag, args) ->
      emit comment (Format.asprintf "AConstruct: %a" ANF.pp_a x);
      emit li a0 (List.length args) ~comm:"size";
      emit li a1 tag;
      emit call "rukaml_alloc_block";
      emit sd_dest a0 dest;
      List.iteri
        (fun i x ->
           helper_a (DReg "t0") x;
           emit sd t0 (ROffset (a0, 8 * i)))
        args
    | AConst (PConst_bool true) ->
      emit li t0 1;
      emit sd_dest t0 dest
    | AConst (PConst_bool false) | AUnit -> emit sd_dest zero dest
    | AConst (PConst_string s) -> assert false
    | APrimitive ("match_failure", _) -> emit call "rukaml_match_failure"
    | _atom ->
      Format.eprintf "Unsupported: @[`%a`@]\n%!" Compile_lib.ANF.pp_a _atom;
      failwiths "not implemented %s %d" __FILE__ __LINE__
  and emit_initialize_block dest ~tag ~fields ~name =
    emit
      comment
      (Format.asprintf
         "Init block with fields: @[[ %a ]@]"
         (pp_space_list ANF.pp_a)
         fields);
    emit li a0 (List.length fields) ~comm:(sprintf "%s size" name);
    emit li a1 tag ~comm:(sprintf "%s tag" name);
    emit call "rukaml_alloc_block";
    (* fresh block stored in a0 *)
    with_two_slots (fun _ block_addr ->
      emit addi SP SP (-16) ~comm:"block_addr :: i :: ...";
      emit sd a0 (Addr_of_local.pp_to_mach block_addr);
      List.iteri
        (fun i -> function
           | ANF.AConst (PConst_int n) ->
             emit li t1 n;
             let comm = sprintf "setting field %d to be const %d" i n in
             emit sd t1 (ROffset (a0, 8 * i)) ~comm
           | ANF.AVar vname when is_toplevel vname = `Local ->
             emit ld t1 (Addr_of_local.pp_to_mach vname);
             emit sd t1 (ROffset (a0, 8 * i))
           | x ->
             helper_a (DReg "t1") x;
             emit ld t0 (Addr_of_local.pp_to_mach block_addr);
             emit sd t1 (ROffset (t0, 8 * i)) ~comm:(sprintf "setting field %d" i);
             ())
        fields;
      emit ld t0 (Addr_of_local.pp_to_mach block_addr);
      emit sd_dest t0 dest;
      emit addi SP SP 16)
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

let prepare_string_lit_init ppf anf =
  let () =
    let iter =
      { ANF.default_iterator with
        cconst_string = (fun _ s -> String_lit_hash.extend s string_list_hash)
      }
    in
    List.iter (fun (_, _, e) -> iter.on_expr iter e) anf
  in
  let assembly_a_string ppf s =
    if String.for_all (fun c -> Char.code c < 128) s
    then printfn ppf ".asciz %S" s
    else (
      let len = String.length s in
      let classify c = if Char.code c < 128 then `letter else `weird in
      let add_weird buf ch = Printf.bprintf buf ".byte 0x%X\n" (Char.code ch) in
      let rec loop i acc =
        if i < len
        then (
          match classify s.[i], acc with
          | `letter, `Letter buf ->
            Buffer.add_char buf s.[i];
            loop (i + 1) acc
          | `weird, `Letter buf ->
            printfn ppf ".ascii \"%s\"" (Buffer.contents buf);
            let buf = Buffer.create 22 in
            add_weird buf s.[i];
            loop (i + 1) (`Weird buf)
          | `weird, `Weird buf ->
            add_weird buf s.[i];
            loop (i + 1) acc
          | `letter, `Weird buf ->
            printfn ppf "%s" (Buffer.contents buf);
            let buf = Buffer.create 20 in
            Buffer.add_char buf s.[i];
            loop (i + 1) (`Letter buf))
        else (
          match acc with
          | `Letter buf -> printfn ppf ".asciz \"%s\"" (Buffer.contents buf)
          | `Weird buf -> printfn ppf "%s" (Buffer.contents buf))
      in
      loop 0 (`Letter (Buffer.create 20)))
  in
  printfn ppf ".data";
  let lit_name n = sprintf "STRING_LIT_%d" n in
  let rukaml_val_loc n = sprintf "my_STRING_LIT_%d" n in
  iter_string_lit_hash (fun k v ->
    printfn ppf "STRING_LIT_%d: %a" v assembly_a_string k;
    printfn ppf ".equ STRING_LIT_%d_len, %d" v (1 + String.length k));
  printfn ppf ".align 3   # Align to 8-byte boundary (2^3)";
  iter_string_lit_hash (fun k v -> printfn ppf "my_STRING_LIT_%d: .quad 0x0 # '%S'" v k);
  fun () ->
    iter_string_lit_hash (fun _k v ->
      emit lla a0 (lit_name v);
      emit call "rukaml_make_string_of_lit";
      emit lla t1 (rukaml_val_loc v);
      emit sd a0 (ROffset (Temp_reg 1, 0)))
;;

module String_set = Set.Make (String)

let codegen ?(wrap_main_into_start = true) anf file =
  let is_toplevel : Ident.t -> def_kind =
    let hash = Hashtbl.create (List.length anf) in
    List.iter
      (fun (_, name, body) ->
         let pats, _ = Compile_lib.ANF.group_abstractions body in
         let argc = List.length pats in
         assert (argc >= 0);
         if argc > 0
         then Hashtbl.add hash name (`Func argc)
         else Hashtbl.add hash name `Val)
      anf;
    fun name ->
      match Hashtbl.find hash name with
      | n -> n
      | exception Not_found -> `Local
  in
  Stdio.Out_channel.with_file file ~f:(fun ch ->
    let ppf = Format.formatter_of_out_channel ch in
    let do_string_init = prepare_string_lit_init ppf anf in
    (* externs *)
    let __ () =
      List.iter
        (printfn ppf "extern %s")
        [ "rukaml_alloc_closure"
        ; "rukaml_print_int"
        ; "rukaml_identity"
        ; "rukaml_alloc_block"
        ; "rukaml_tag"
        ; "rukaml_alloc_array"
        ; "rukaml_array_length"
        ; "rukaml_array_get"
        ; "rukaml_array_set"
        ; "rukaml_array_stdin"
        ; "rukaml_array_read_in"
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
    (* if wrap_main_into_start
    then (
      printfn ppf "";
      printfn ppf ".text";
      printfn ppf ".globl _start";
      printfn ppf "_start:";
      emit call "main";
      print_epilogue ppf "main";
      Machine.flush_queue ppf;
      ()); *)
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
        List.filter_map
          (function
            | ANF.APname name -> Some name
            | ANF.APunit -> None)
          pats
      in
      let () =
        if name.Ident.hum_name = "main"
        then (
          emit mv a0 sp;
          emit call "rukaml_initialize";
          emit comment "this is main";
          do_string_init ();
          emit li a0 0)
        else
          List.rev pats
          |> ListLabels.iteri ~f:(fun i -> function
            | ANF.APname name -> Addr_of_local.add_arg ~argc i name
            | APunit -> ())
      in
      generate_body is_toplevel body;
      Addr_of_local.remove_args names;
      print_epilogue ppf name.hum_name;
      Machine.flush_queue ppf
    in
    printfn ppf "\n.text";
    List.iter on_vb anf;
    Format.pp_print_flush ppf ());
  Result.Ok ()
;;
