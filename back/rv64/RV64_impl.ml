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

  let is_empty h = length h = 0
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

(* TODO: implement as a functor? *)

let march : [ `RV64 | `RV32 ] ref = ref `RV64
let enable_32 () = march := `RV32

(** Returns word size in bytes *)
let wordsize () =
  match !march with
  | `RV64 -> 8
  | `RV32 -> 4
;;

open Frontend
module Toplevel = Amd64_impl.Toplevel

type dest =
  | DReg of string
  | DStack_var of Ident.t

let make_sp_offset offset = Machine.(ROffset (SP, offset * wordsize ()))

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
    if offset = 0 then fprintf ppf "(sp)" else fprintf ppf "%d(sp)" (offset * wordsize ())
  ;;

  (* 8 for 64 bit, 4 for 32bit *)

  let pp_to_mach name =
    let offset =
      let o = find_exn name in
      if o > 0 then !last_pos - o else !last_pos - o
    in
    if not (offset >= 0) then assert (offset >= 0);
    make_sp_offset offset
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

let sd k a b =
  match !march with
  | `RV64 -> Machine.sd k a b
  | `RV32 -> Machine.sw k a b
;;

let ld k a b =
  match !march with
  | `RV64 -> Machine.ld k a b
  | `RV32 -> Machine.lw k a b
;;

let mulw k a b c =
  match !march with
  | `RV64 -> Machine.mulw k a b c
  | `RV32 -> Machine.mul k a b c
;;

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
    | ELet (_flg, Apat_var name, rhs, where_) ->
      local_names := Ident.Ident_set.add name !local_names;
      helper_c rhs;
      helper where_
    | ELet (_flg, Apat_unit, rhs, where_) ->
      helper_c rhs;
      helper where_
    | ELet _ as expr ->
      Format.eprintf "%a\n%!" ANF.pp expr;
      failwiths "Not implemented %s %d" __FILE__ __LINE__
  and helper_c = function
    | CIte (_, th, el) ->
      helper th;
      helper el
    | CConstruct _ | CTuple _ | CApp _ | CAtom _ -> ()
  in
  helper input_anf;
  let local_names = Ident.Ident_set.to_list !local_names in
  let count = List.length local_names in
  (* If assertion fails it's like a number of locals with the same names *)
  let args_repr = Ident.concat_str local_names in
  let ra_offset =
    let sp_offset = if count mod 2 = 0 then count + 2 else count + 1 in
    emit addi sp sp (-sp_offset * wordsize ());
    Addr_of_local.last_pos := !Addr_of_local.last_pos + sp_offset - count;
    ListLabels.iter (List.rev local_names) ~f:(fun name -> Addr_of_local.extend name);
    ListLabels.iter local_names ~f:(fun name ->
      let comm = Format.asprintf "loc for %a" Ident.pp name in
      emit sd zero (Addr_of_local.pp_to_mach name) ~comm);
    emit sd ra (ROffset (SP, count * wordsize ()));
    if count mod 2 = 0
    then (
      let comm = "padding" in
      emit sd zero (ROffset (SP, (1 + count) * wordsize ())) ~comm);
    wordsize () * count
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
          (wordsize () * (count + 2))
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
          (wordsize () + (wordsize () * count))
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
  emit addi sp sp (-wordsize ()) ~comm:(sprintf "alloc space for RA register");
  let rez = f ra_temp_name in
  emit ld ra (ROffset (SP, 0));
  emit addi sp sp (wordsize ()) ~comm:"free space of RA register";
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

let with_ra_saving f =
  let slot1 = Ident.of_string @@ Printf.sprintf "x%d" (gensym ()) in
  let slot2 = Ident.of_string @@ Printf.sprintf "x%d" (gensym ()) in
  Addr_of_local.extend slot1;
  Addr_of_local.extend slot2;
  emit addi SP SP (-2 * wordsize ());
  emit sd ra (ROffset (SP, 0));
  let () = f () in
  emit ld ra (ROffset (SP, 0));
  emit addi SP SP (2 * wordsize ());
  Addr_of_local.remove_local slot2;
  Addr_of_local.remove_local slot1
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

(** Functions [grow_stack] and [shrink_stack] take words count *)
let grow_stack k n =
  assert (n > 0);
  addi k sp sp (-n * wordsize ())
;;

let shrink_stack k n =
  assert (n > 0);
  addi k sp sp (n * wordsize ())
;;

let emit_alloc_closure fname arity =
  emit lla (RU "a0") fname;
  emit li (RU "a1") arity;
  emit call "rukaml_alloc_closure"
;;

let pp_to_mach = Addr_of_local.pp_to_mach

(**
    Argument [is_toplevel] returns None or Some arity. *)
let generate_body is_toplevel body =
  let open Parsetree in
  let dealloc_locals, locals = allocate_locals body in
  let deallocate_args_for_call argc =
    let padded_argc : int = if argc mod 2 = 0 then argc else argc + 1 in
    Addr_of_local.(last_pos := !last_pos - padded_argc);
    emit addi SP SP (wordsize () * padded_argc) ~comm:(sprintf "deallocate %d args" argc)
  in
  let allocate_args_for_call ?f args =
    (* Allocate args for a function call inside a body *)
    let count = List.length args in
    emit
      comment
      (sprintf "Allocate args to call fun %S with args" (Option.get f).Ident.hum_name);
    let stack_slots = if count mod 2 = 0 then count else 1 + count in
    Addr_of_local.last_pos := !Addr_of_local.last_pos + stack_slots;
    emit grow_stack stack_slots ~comm:(sprintf "last_pos = %d" !Addr_of_local.last_pos);
    (* TODO(Kakadu): check RTL *)
    (* TODO(Kakadu): Rwrite to emit less code *)
    let pp_access ?(doc = "") v offset =
      emit li t0 v;
      emit sd t0 (ROffset (SP, wordsize () * offset)) ~comm:doc
    in
    let on_arg i arg =
      (* iteration is RTL *)
      match arg with
      | Compile_lib.ANF.AUnit | AConst (PConst_bool false) -> pp_access 0 i
      | AConst (PConst_bool true) -> pp_access 1 i
      | AConst (PConst_int n) -> pp_access ~doc:"constant" n i
      | AConst (PConst_char c) -> pp_access ~doc:"constant" (Char.code c) i
      | AConst (PConst_string s) ->
        let rukaml_val_loc n = sprintf "my_STRING_LIT_%d" n in
        emit lla t0 (rukaml_val_loc (String_lit_hash.find string_list_hash s));
        emit ld t0 (ROffset (Temp_reg 0, 0));
        emit sd t0 (make_sp_offset i) ~comm:"string literal"
      | AVar vname
        when Toplevel.find_opt vname <> None (* Option.is_some (is_toplevel vname)  *) ->
        (match Toplevel.find_exn vname with
         | { kind = Toplevel.Function { argc = arity }; _ } ->
           emit_alloc_closure vname.hum_name arity;
           emit sd a0 (make_sp_offset i)
         | { kind = Main; _ } -> failwith "Should not happen"
         | { kind = Alias _; _ } -> failwith "Not implemented"
         | { ident; kind = Immediate Constant } ->
           assert (Ident.equal ident vname);
           emit ld t0 (RU (Format.asprintf "%a" Toplevel.pp_label_exn ident));
           emit comment "imm constant";
           emit sd t0 (make_sp_offset i)
         | { kind = Immediate Eval; _ } ->
           failwiths "Not implemented %s %d" __FILE__ __LINE__
         | { kind = Immediate Match; _ } ->
           failwiths "Not implemented %s %d" __FILE__ __LINE__)
      | AVar vname ->
        (* TODO: use pp_access *)
        emit ld t0 (pp_to_mach vname) ~comm:(sprintf "arg %S" vname.hum_name);
        emit sd t0 (make_sp_offset i)
      | APrimitive ("char_code", 1) ->
        emit_alloc_closure "rukaml_identity" 1;
        emit sd a0 (make_sp_offset i)
      | APrimitive ("array_len", 1) ->
        emit_alloc_closure "rukaml_array_length" 1;
        emit sd a0 (make_sp_offset i)
      | APrimitive ("array_get", 2) ->
        emit_alloc_closure "rukaml_array_get" 2;
        emit sd a0 (make_sp_offset i)
      | APrimitive ("array_set", 3) ->
        emit_alloc_closure "rukaml_array_set" 3;
        emit sd a0 (make_sp_offset i)
      (* | AVar { Ident.hum_name = "stdin"; _ } -> emit call "rukaml_array_stdin" *)
      (* Result is in a0 *)
      | ALam _ -> failwith "Should it be representable in ANF?"
      | APrimitive ("print", (1 as parity)) ->
        emit_alloc_closure "rukaml_print_int_kaml" parity;
        emit sd a0 (make_sp_offset i)
      | APrimitive ("stdout", 0) -> pp_access 1 i
      | APrimitive ("sys_argv", 0) ->
        emit call "rukaml_get_argv";
        emit sd a0 (make_sp_offset i)
      | APrimitive (_, arity) as arg ->
        failwiths "Primitive %a/%d is not supported" ANF.pp_a arg arity
      | AArray _ -> failwiths "Arrays are not atomic. Fix and implement this TODO"
    in
    ListLabels.iteri args ~f:on_arg;
    count
  in
  let is_unary_prim, on_unary_prim =
    let mangling = String_lit_hash.create 34 in
    String_lit_hash.add mangling "close_out" "rukaml_close_out";
    String_lit_hash.add mangling "rukaml_input_all" "rukaml_input_all";
    String_lit_hash.add mangling "gc_compact" "rukaml_gc_compact";
    String_lit_hash.add mangling "gc_stats" "rukaml_gc_stats";
    let checker str = String_lit_hash.mem mangling str in
    let codegen helper_a ?(sysv = true) ident arg0 dest =
      helper_a (DReg "a0") arg0;
      let ans = String_lit_hash.find mangling ident in
      let ans = if sysv then ans ^ "_sysv" else ans in
      emit call ans;
      emit sd_dest a0 dest
    in
    checker, codegen
  in
  let rec helper dest = function
    | Compile_lib.ANF.EComplex c -> helper_c dest c
    | ELet (_, Apat_var name, rhs, wher) ->
      assert (Addr_of_local.contains name);
      let local = DStack_var name in
      helper_c local rhs;
      helper dest wher
    | ELet (_, Apat_unit, rhs, wher) ->
      helper_c (DReg "zero") rhs;
      helper dest wher
    | ELet _ as anf -> failwiths "Not implemented: @[%a@]" ANF.pp anf
  and helper_c (dest : dest) = function
    | CIte (CAtom (AConst (Parsetree.PConst_bool true)), bth, _bel) -> helper dest bth
    | CIte (CAtom (AConst (Parsetree.PConst_bool false)), _bth, bel) -> helper dest bel
    (* if-then-else with string equality *)
    | CIte
        ( CApp (APrimitive ("=", 2), AVar vname, [ AConst (Parsetree.PConst_string str) ])
        , bth
        , bel ) ->
      emit ld t0 (pp_to_mach vname);
      let rukaml_val_loc n = sprintf "my_STRING_LIT_%d" n in
      emit lla a1 (rukaml_val_loc (String_lit_hash.find string_list_hash str));
      emit ld a1 (ROffset (a1, 0));
      emit mv t0 a0;
      emit call "rukaml_equal_sysv";
      emit mv t0 a0;
      let el_lab = Printf.sprintf "lab_else_%d" (gensym ()) in
      let fin_lab = Printf.sprintf "lab_endif_%d" (gensym ()) in
      emit beq t0 zero el_lab;
      helper dest bth;
      emit beq zero zero fin_lab;
      emit label el_lab;
      helper dest bel;
      emit label fin_lab
    | CIte
        ( CApp
            ( APrimitive ("=", 2)
            , AConst (Parsetree.PConst_int l)
            , [ AConst (Parsetree.PConst_int r) ] )
        , bth
        , bel ) ->
      (* This is not entirely correct, because OCaml number and target could be different *)
      helper dest (if l = r then bth else bel)
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
            , ((AConst (PConst_int _) | AConst (PConst_char _) | AVar _) as lhs)
            , [ ((AConst (PConst_int _) | AConst (PConst_char _) | AVar _) as rhs) ] )
        , bthen
        , belse ) ->
      helper_a (DReg "t0") lhs;
      (* emit ld t0 (pp_to_mach vname) ~comm:(Format.asprintf "access %a" Ident.pp vname);
      emit li t1 n; *)
      helper_a (DReg "t1") rhs;
      let lab_then = Printf.sprintf "lab_then_%d" (gensym ()) in
      let lab_fin = Printf.sprintf "lab_fin_%d" (gensym ()) in
      let op_mnem =
        match op with
        | "<" -> blt
        | "=" -> beq
        | "<=" -> ble
        | _ -> failwiths "Should not happen %s %d" __FILE__ __LINE__
      in
      emit op_mnem t0 t1 lab_then;
      helper dest belse;
      emit beq zero zero lab_fin;
      emit label lab_then;
      helper dest bthen;
      emit label lab_fin
    (* | CApp (AVar f, arg1, [])
      when f.Ident.hum_name = "char_code"
           && is_toplevel f = None
           && not (Addr_of_local.has_key f) ->
      (match arg1 with
       | AVar v when Addr_of_local.has_key v ->
         emit ld t0 (pp_to_mach v);
         emit sd_dest t0 dest
       | AConst (PConst_char c) ->
         emit li t0 (Char.code c);
         emit sd_dest t0 dest
       | _ -> failwith "Should not happen: char_code") *)
    (* | CAtom (AVar f)
      when f.Ident.hum_name = "stdin"
           && is_toplevel f = None
           && not (Addr_of_local.has_key f) ->
      emit call "rukaml_array_stdin";
      emit sd_dest a0 dest *)
    (* | CApp (AVar f, arg1, [])
      when f.Ident.hum_name = "open_in"
           && is_toplevel f = None
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
           emit shrink_stack 2)
       | AArray _ as r ->
         with_two_slots (fun ra_name arg_name ->
           emit addi SP SP (-16);
           emit sd ra (pp_to_mach ra_name);
           emit sd a0 (pp_to_mach arg_name);
           helper_a (DReg "a0") r;
           emit call "rukaml_array_read_in";
           emit ld ra (pp_to_mach ra_name);
           emit sd_dest a0 dest;
           emit shrink_stack 2)
       | _ -> failwith "Should not happen: open_in") *)
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
       | AConst (PConst_bool _)
       | AConst (PConst_char _)
       | AArray _ | AVar _ | APrimitive _
       | ALam (_, _)
       | AUnit -> failwith "Should not happen: print_int"
       | _ -> failwiths "not implemented %s %d" __FILE__ __LINE__)
    | CApp (APrimitive ("string_nth", 2), arg1, [ arg2 ]) ->
      helper_a (DReg "a0") arg1;
      helper_a (DReg "a1") arg2;
      emit call "rukaml_string_nth_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("string_len", 1), arg1, []) ->
      helper_a (DReg "a0") arg1;
      emit call "rukaml_string_length_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("string_of_char_list", 1), arg1, []) ->
      helper_a (DReg "a0") arg1;
      emit call "rukaml_string_of_char_list_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("array_len", 1), arg1, []) ->
      (match arg1 with
       | AVar v when Addr_of_local.has_key v ->
         with_two_slots (fun ra_name arg_name ->
           emit grow_stack 2;
           emit sd ra (pp_to_mach ra_name);
           emit ld t0 (pp_to_mach v);
           emit mv (pp_to_mach arg_name) t0;
           emit call "rukaml_array_length";
           emit ld ra (pp_to_mach ra_name);
           emit sd_dest a0 dest;
           emit shrink_stack 2)
       | AArray _ | _ -> failwith "Should not happen")
    | CApp (APrimitive ("array_get", 2), arg1, []) ->
      (match arg1 with
       | AVar arr when Addr_of_local.has_key arr ->
         with_ra_saving (fun () ->
           emit_alloc_closure "rukaml_array_get" 2;
           emit li a1 1;
           emit ld a2 (pp_to_mach arr);
           emit call "rukaml_applyN";
           emit sd_dest a0 dest)
       | _ -> failwiths "Should not happen %s %d" __FILE__ __LINE__)
    | CApp (APrimitive ("array_get", 2), arg1, [ arg2 ]) ->
      helper_a (DReg "a0") arg1;
      helper_a (DReg "a1") arg2;
      emit call "rukaml_array_get_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("array_set", 3), arg1, []) as e ->
      (match arg1 with
       | AVar arr when Addr_of_local.has_key arr ->
         with_ra_saving (fun () ->
           emit_alloc_closure "rukaml_array_set" 3;
           emit li a1 1;
           emit ld a2 (pp_to_mach arr);
           emit call "rukaml_applyN";
           emit sd_dest a0 dest)
       | _ ->
         Format.eprintf "Error: @[%a@]\n%!" ANF.pp_c e;
         failwiths "Should not happen %s %d" __FILE__ __LINE__)
    | CApp (APrimitive ("array_set", 3), arg1, [ arg2; arg3 ]) ->
      (* it's better to use registers for passing args *)
      let ra_slot = Ident.of_string @@ Printf.sprintf "ra%d" (gensym ()) in
      let item_slot = Ident.of_string @@ Printf.sprintf "item%d" (gensym ()) in
      let pos_slot = Ident.of_string @@ Printf.sprintf "pos%d" (gensym ()) in
      let arr_slot = Ident.of_string @@ Printf.sprintf "arr%d" (gensym ()) in
      Addr_of_local.extend ra_slot (* 24(sp) *);
      Addr_of_local.extend item_slot (* 16(sp) *);
      Addr_of_local.extend pos_slot (* 8(sp) *);
      Addr_of_local.extend arr_slot (* 0 (sp) *);
      emit addi SP SP (-4 * wordsize ());
      emit sd ra (pp_to_mach ra_slot);
      helper_a (DReg "a0") arg1;
      helper_a (DReg "a1") arg2;
      helper_a (DReg "a2") arg3;
      (* (match arg1 with
       | AVar arr ->
         emit ld a0 (pp_to_mach arr);
         emit sd a0 (pp_to_mach arr_slot)
       | _ -> assert false);
      (match arg2 with
       | AVar nvar ->
         emit ld a0 (pp_to_mach nvar);
         emit sd a0 (pp_to_mach pos_slot)
       | AConst (PConst_int n) ->
         emit li a0 n;
         emit sd a0 (pp_to_mach pos_slot)
       | _ -> assert false);
      helper_a (DStack_var item_slot) arg3; *)
      emit call "rukaml_array_set_sysv";
      emit ld ra (pp_to_mach ra_slot);
      emit addi SP SP (4 * wordsize ());
      Addr_of_local.remove_local arr_slot;
      Addr_of_local.remove_local pos_slot;
      Addr_of_local.remove_local item_slot;
      Addr_of_local.remove_local ra_slot;
      emit sd_dest a0 dest
    | CApp (APrimitive ("+", _), AConst (PConst_int l), [ AConst (PConst_int r) ]) ->
      emit li t0 (l + r);
      emit sd_dest t0 dest
    | CApp (APrimitive ("-", _), AConst (PConst_int l), [ AConst (PConst_int r) ]) ->
      emit li t0 (l - r);
      emit sd_dest t0 dest
    | CApp (APrimitive ("*", _), AConst (PConst_int l), [ AConst (PConst_int r) ]) ->
      emit li t0 (l * r);
      emit sd_dest t0 dest
    | CApp (APrimitive ("=", _), AConst (PConst_int l), [ AConst (PConst_int r) ]) ->
      (* TODO: user Addr_of_local.pp_local_exn *)
      if l = r
      then (
        emit li t0 1;
        emit sd_dest t0 dest)
      else failwiths "not implemented %d" __LINE__
    | CApp (APrimitive ("&&", _), al, [ ar ]) ->
      let on_arg dest = function
        | ANF.AVar vname -> emit ld dest (pp_to_mach vname)
        | _ -> failwiths "Not implemented: %s %d" __FILE__ __LINE__
      in
      on_arg t0 al;
      on_arg t1 ar;
      emit and_ t0 t0 t1;
      emit sd_dest t0 dest
    | CApp (APrimitive (("<" as op), _), AVar vname, [ AConst (PConst_int n) ]) ->
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
    | CApp (APrimitive ("-", 2), arg1, [ arg2 ]) ->
      helper_a (DReg "t4") arg1;
      helper_a (DReg "t5") arg2;
      emit sub t4 t4 t5;
      emit sd_dest t4 dest
    (* | CApp (APrimitive ("-", 2), AVar vname, [ AConst (PConst_int n) ]) ->
      (match is_toplevel vname with
       | None ->
         emit ld t5 (pp_to_mach vname);
         emit addi t5 t5 (-n);
         emit sd_dest t5 dest
       | Some _ ->
         (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
         failwiths "not implemented %d" __LINE__) *)
    | CApp (APrimitive ((("+" | "*") as prim), _), AVar vname, [ AConst (PConst_int n) ])
    | CApp (APrimitive ((("+" | "*") as prim), _), AConst (PConst_int n), [ AVar vname ])
      ->
      (match is_toplevel vname with
       | None ->
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
       | Some _ ->
         (* TODO: This will be fixed when we will allow toplevel non-functional constants *)
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
    | CApp (APrimitive ("%int_equality", _), AVar vl, [ AVar vr ]) ->
      helper_a (DReg "t5") (AVar vl);
      helper_a (DReg "t6") (AVar vr);
      emit sub t0 t5 t6;
      (* Unsigned integer <1 is only zero *)
      emit sltiu t0 t0 1;
      emit sd_dest t0 dest
    | CApp (APrimitive ("string_equal", 2), vl, [ vr ])
    | CApp (APrimitive ("=", _), vl, [ vr ]) ->
      (* This case is complicated when arguments are non immediate. *)
      (* TODO: add explicit primitive %rukaml_equal? *)
      helper_a (DReg "t5") vl;
      helper_a (DReg "t6") vr;
      emit addi a0 t5 0;
      emit addi a1 t6 0;
      emit call "rukaml_equal_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive (">", arity), vl, [ vr ]) ->
      helper_c dest (CApp (APrimitive ("<", arity), vr, [ vl ]))
    | CApp (APrimitive ("<", _), vl, [ vr ]) ->
      (* This case is complicated when arguments are non immediate. *)
      (* TODO: add explicit primitive %rukaml_equal? *)
      helper_a (DReg "t5") vl;
      helper_a (DReg "t6") vr;
      emit addi a0 t5 0;
      emit addi a1 t6 0;
      emit call "rukaml_compare_sysv";
      emit slti t5 a0 0;
      emit sd_dest t5 dest
    | CApp (APrimitive ((("+" | "*" | "-" | "||") as prim), _), AVar vl, [ AVar vr ]) ->
      emit comment (sprintf "%s is stored in %d" vl.hum_name (Addr_of_local.find_exn vl));
      emit comment (sprintf "%s is stored in %d" vr.hum_name (Addr_of_local.find_exn vr));
      emit comment (sprintf "last_pos = %d" !Addr_of_local.last_pos);
      emit ld t3 (Addr_of_local.pp_to_mach vl);
      emit ld t4 (Addr_of_local.pp_to_mach vr);
      (match prim with
       | "+" -> emit add
       | "*" -> emit mulw
       | "-" -> emit sub
       | "||" -> emit or_
       | op -> failwiths "not_implemeted  %S. %d" op __LINE__)
        t5
        t3
        t4;
      emit sd_dest t5 dest
    | CApp (APrimitive ("trace_rukaml_val", 1), arg, []) ->
      helper_a (DReg "a0") arg;
      emit li (RU "a1") 0;
      emit call "rukaml_trace_val";
      if dest <> DReg "a0" then emit sd_dest (RU "a0") dest
    | CApp (AVar f, arg1, args) when Toplevel.is_toplevel_function f ->
      emit comment "HERR: use new Toplevel module";
      (* Calling a rukaml function uses custom calling convention.
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
          emit grow_stack 2 ~comm:(sprintf " RA + closure");
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
                 emit ~comm:(sprintf "arg %d" i) ld (RU rname) (make_sp_offset i))
              (list_take formal_arity [ (*"a0"; *) "a2"; "a3"; "a4"; "a5" ])
          in
          emit call "rukaml_applyN";
          emit sd_dest (RU "a0") dest;
          deallocate_args_for_call formal_arity;
          emit ld ra (Addr_of_local.pp_to_mach ra_name);
          emit shrink_stack 2 ~comm:"deallocate RA + closure")
      else failwith "Arity mismatch: over application"
    | CApp (AVar f, (AConst _ as arg), []) | CApp (AVar f, (AVar _ as arg), []) ->
      (* A 1 argument application *)
      assert (Option.is_none (is_toplevel f));
      helper_a (DReg "a2") arg;
      emit ld (RU "a0") (Addr_of_local.pp_to_mach f);
      emit li (RU "a1") 1;
      emit call "rukaml_applyN";
      if dest <> DReg "a0" then emit sd_dest (RU "a0") dest
    | CApp (APrimitive ("field", _), AConst (PConst_int _n), [ AVar _ ]) ->
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
    | CApp (APrimitive ("char_code", 1), AVar arg, []) ->
      emit ld t0 (pp_to_mach arg);
      emit sd_dest t0 dest
    | CApp (APrimitive ("block_tag", _), AVar arg, []) ->
      emit ld a0 (pp_to_mach arg);
      emit call "rukaml_tag0";
      emit sd_dest a0 dest ~comm:(Format.asprintf "got tag of '%a'" Ident.pp arg)
    | CApp (APrimitive ("block_nth", _), AVar from, [ AConst (PConst_int idx) ])
      when Addr_of_local.has_key from ->
      (* with_ra_saving (fun () ->
        emit li a0 idx;
        emit ld a1 (pp_to_mach from);
        emit call "rukaml_field";
        emit sd_dest a0 dest) *)
      emit
        ld
        a1
        (pp_to_mach from)
        ~comm:(Format.asprintf "block_nth: from = '%a'" Ident.pp from);
      emit li a0 idx;
      emit call "rukaml_field";
      emit sd_dest a0 dest
    | CApp (APrimitive ("match_failure", 1), _, []) ->
      with_ra_saving (fun () ->
        emit li a0 0;
        emit call "rukaml_match_failure")
    | CApp (APrimitive ("sprintf", 1), AVar arg0, []) ->
      emit ld a0 (pp_to_mach arg0);
      emit call "rukaml_alloc_sprintf_closure_sysv" ~comm:"sprintf var";
      emit sd_dest a0 dest
    | CApp (APrimitive ("sprintf", 1), AConst (Parsetree.PConst_string fstr), []) ->
      let rukaml_val_loc n = sprintf "my_STRING_LIT_%d" n in
      emit lla t0 (rukaml_val_loc (String_lit_hash.find string_list_hash fstr));
      emit ld a0 (ROffset (Temp_reg 0, 0));
      emit call "rukaml_alloc_sprintf_closure_sysv" ~comm:"sprintf const";
      emit sd_dest a0 dest
    | CApp (APrimitive ("printf", 1), AVar arg0, []) ->
      emit ld a0 (pp_to_mach arg0);
      emit call "rukaml_alloc_printf_closure0";
      emit sd_dest a0 dest
    | CApp (APrimitive ("printf", 1), AConst (Parsetree.PConst_string fstr), []) ->
      let rukaml_val_loc n = sprintf "my_STRING_LIT_%d" n in
      emit lla t0 (rukaml_val_loc (String_lit_hash.find string_list_hash fstr));
      emit ld a0 (ROffset (Temp_reg 0, 0));
      emit call "rukaml_alloc_printf_closure0";
      emit sd_dest a0 dest
    | CApp (APrimitive ("fprintf", 2), APrimitive ("stdout", 0), [ AVar arg1 ]) ->
      emit li a0 1;
      emit ld a1 (pp_to_mach arg1);
      emit call "rukaml_alloc_fprintf_closure_sysv";
      emit sd_dest a0 dest
    | CApp
        ( APrimitive ("fprintf", 2)
        , ((AVar _ | APrimitive _) as arg0)
        , [ AConst (Parsetree.PConst_string fstr) ] ) ->
      helper_a (DReg "a0") arg0;
      let rukaml_val_loc n = sprintf "my_STRING_LIT_%d" n in
      emit lla t0 (rukaml_val_loc (String_lit_hash.find string_list_hash fstr));
      emit ld a1 (ROffset (Temp_reg 0, 0));
      emit call "rukaml_alloc_fprintf_closure_sysv";
      emit sd_dest a0 dest
    | CApp
        ( APrimitive ("fprintf", 2)
        , ((AVar _ | APrimitive _) as arg0)
        , [ (AVar _ as arg1) ] ) ->
      helper_a (DReg "a0") arg0;
      helper_a (DReg "a1") arg1;
      emit call "rukaml_alloc_fprintf_closure_sysv";
      emit sd_dest a0 dest
    | CApp
        ( APrimitive ("output_string", 2)
        , ((APrimitive ("stdout", 0) | AVar _) as arg0)
        , [ AConst (Parsetree.PConst_string str) ] ) ->
      let rukaml_val_loc n = sprintf "my_STRING_LIT_%d" n in
      emit lla t0 (rukaml_val_loc (String_lit_hash.find string_list_hash str));
      emit ld a1 (ROffset (t0, 0));
      helper_a (DReg "a0") arg0;
      emit call "rukaml_output_string_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("output_string", 2), APrimitive ("stdout", 0), [ AVar arg1 ]) ->
      emit ld a1 (pp_to_mach arg1);
      emit li a0 1;
      emit call "rukaml_output_string_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("output_string", 2), AVar arg0, [ AVar arg1 ]) ->
      emit ld a1 (pp_to_mach arg1);
      emit ld a0 (pp_to_mach arg0);
      emit call "rukaml_output_string_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("exit", 1), arg0, []) ->
      helper_a (DReg "a0") arg0;
      emit call "rukaml_sys_exit_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("end_of_input", 1), arg0, []) ->
      helper_a (DReg "a0") arg0;
      emit call "rukaml_end_of_input_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("input_char", 1), arg0, []) ->
      helper_a (DReg "a0") arg0;
      emit call "rukaml_input_char_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("open_in", 1), arg0, []) ->
      helper_a (DReg "a0") arg0;
      emit call "rukaml_open_in_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("close_in", 1), arg0, []) ->
      helper_a (DReg "a0") arg0;
      emit call "rukaml_close_in_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("open_out", 1), arg0, []) ->
      helper_a (DReg "a0") arg0;
      emit call "rukaml_open_out_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("close_out", 1), arg0, []) ->
      helper_a (DReg "a0") arg0;
      emit call "rukaml_close_out_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive ("char_code", 1), AConst (PConst_char c), []) ->
      emit li t5 (Char.code c);
      emit sd_dest t5 dest
    | CApp (APrimitive ("substring", 3), arg1, [ arg2; arg3 ]) ->
      helper_a (DReg "a0") arg1;
      helper_a (DReg "a1") arg2;
      helper_a (DReg "a2") arg3;
      emit call "rukaml_substring_sysv";
      emit sd_dest a0 dest
    | CApp (APrimitive (name, 1), arg1, []) when is_unary_prim name ->
      on_unary_prim helper_a ~sysv:true name arg1 dest
    | CApp (APrimitive (pname, partiy), arg1, args) ->
      Format.eprintf "Unsupported primitive call: %s/%d\n%!" pname partiy;
      Format.eprintf " args = %a\n%!" (pp_space_list ANF.pp_a) (arg1 :: args);
      failwiths "Not implemented %d" __LINE__
    | CApp _ as anf ->
      Format.eprintf "Unsupported: @[`%a`@]\n%!" Compile_lib.ANF.pp_c anf;
      failwiths "Not implemented %d" __LINE__
    | CTuple (x1, x2, xs) ->
      emit_initialize_block dest ~fields:(x1 :: x2 :: xs) ~tag:0 ~name:"tuple"
    | CConstruct (tag, args) ->
      with_two_slots (fun _ra_name rez_slot ->
        emit addi SP SP (-2 * wordsize ());
        (* emit sd (RU "ra") (pp_to_mach ra_name); *)
        emit li a0 (List.length args);
        emit li a1 tag;
        emit call "rukaml_alloc_block" ~comm:(sprintf "argsc = %d" (List.length args));
        emit sd a0 (pp_to_mach rez_slot);
        List.iteri
          (fun i x ->
             helper_a (DReg "t0") x;
             emit comment (Format.asprintf "@[%a@]" ANF.pp_a x);
             emit ld t1 (pp_to_mach rez_slot);
             emit sd t0 (ROffset (t1, i * wordsize ())))
          args;
        (* emit ld ra (pp_to_mach ra_name); *)
        emit ld t0 (pp_to_mach rez_slot);
        emit sd_dest t0 dest;
        emit addi SP SP (2 * wordsize ()))
    | _rest ->
      Format.eprintf "@[%a@]\n%!" Compile_lib.ANF.pp_c _rest;
      failwiths "Not implemented %s %d" __FILE__ __LINE__
  and helper_a (dest : dest) x =
    (* log "  %s: expr = %a" __FUNCTION__ ANF.pp_a x; *)
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
      (match Toplevel.find_opt vname with
       | Some { kind = Function { argc = 0 }; _ } -> assert false
       | Some { kind = Function { argc }; _ } ->
         assert (argc > 0);
         (* failwith "TODO: create a closure" *)
         emit_alloc_closure vname.hum_name argc;
         emit sd_dest (RU "a0") dest
       | Some { kind = Immediate Constant; ident } ->
         emit ld t0 (RU (Format.asprintf "%a" Toplevel.pp_label_exn ident));
         emit comment "imm constant";
         emit sd_dest (RU "t0") dest
       | Some { kind = Main; _ } -> assert false
       | Some { kind = Alias _; _ } -> assert false
       | Some { kind = Immediate Eval; _ } -> assert false
       | Some { kind = Immediate Match; _ } -> assert false
       | None ->
         emit ld t0 (Addr_of_local.pp_to_mach vname);
         (match dest with
          | DReg _ -> emit addi1dest dest t0 0
          | DStack_var _ ->
            emit sd_dest t0 dest ~comm:(sprintf "access a var %S" vname.hum_name)))
      (* (match is_toplevel vname with
       | None ->
         emit ld t5 (Addr_of_local.pp_to_mach vname);
         (match dest with
          | DReg _ -> emit addi1dest dest t5 0
          | DStack_var _ ->
            emit sd_dest t5 dest ~comm:(sprintf "access a var %S" vname.hum_name))
       | Some arity ->
         emit_alloc_closure vname.hum_name arity;
         emit sd_dest (RU "a0") dest) *)
    | AArray r ->
      with_two_slots (fun ra_name arr_slot ->
        emit grow_stack 2;
        emit sd (RU "ra") (pp_to_mach ra_name);
        emit li a0 (List.length r);
        emit call "rukaml_alloc_array";
        emit sd a0 (pp_to_mach arr_slot);
        List.iteri
          (fun i x ->
             helper_a (DReg "t0") x;
             emit ld t1 (pp_to_mach arr_slot);
             (* TODO: Use wordsize below *)
             emit sd t0 (ROffset (t1, i * wordsize ())))
          r;
        emit ld ra (pp_to_mach ra_name);
        emit ld t0 (pp_to_mach arr_slot);
        emit sd_dest t0 dest;
        emit shrink_stack 2)
    | AConst (PConst_bool true) ->
      emit li t0 1;
      emit sd_dest t0 dest
    | AConst (PConst_bool false) | AUnit -> emit sd_dest zero dest
    | AConst (PConst_string s) ->
      let rukaml_val_loc n = sprintf "my_STRING_LIT_%d" n in
      emit lla t0 (rukaml_val_loc (String_lit_hash.find string_list_hash s));
      emit ld t0 (ROffset (Temp_reg 0, 0));
      emit sd_dest t0 dest
    | APrimitive ("match_failure", _) -> emit call "rukaml_match_failure"
    | APrimitive ("stdin", 0) ->
      (match dest with
       | DReg rname -> emit li (RU rname) 1
       | DStack_var _ ->
         emit li t0 0;
         emit sd_dest t0 dest)
    | APrimitive ("stdout", 0) ->
      (match dest with
       | DReg rname -> emit li (RU rname) 1
       | DStack_var _ ->
         emit li t0 1;
         emit sd_dest t0 dest)
    | APrimitive ("stderr", 0) ->
      (match dest with
       | DReg rname -> emit li (RU rname) 2
       | DStack_var _ ->
         emit li t0 2;
         emit sd_dest t0 dest)
    | _atom ->
      Format.eprintf "Unsupported atom: @[`%a`@]\n%!" Compile_lib.ANF.pp_a _atom;
      failwiths "not implemented %s %d" __FILE__ __LINE__
  and emit_initialize_block dest ~tag ~fields ~name =
    emit
      comment
      (Format.asprintf
         "Init tuple with fields: @[[ %a ]@]"
         (pp_space_list ANF.pp_a)
         fields);
    emit li a0 (List.length fields) ~comm:(sprintf "%s size" name);
    emit li a1 tag ~comm:(sprintf "%s tag" name);
    emit call "rukaml_alloc_block";
    (* fresh block stored in a0 *)
    with_two_slots (fun _ block_addr ->
      emit grow_stack 2 ~comm:"block_addr :: i :: ...";
      emit sd a0 (Addr_of_local.pp_to_mach block_addr);
      List.iteri
        (fun i -> function
           | ANF.AConst (PConst_int n) ->
             emit li t1 n;
             let comm = sprintf "setting field %d to be const %d" i n in
             emit sd t1 (ROffset (a0, i * wordsize ())) ~comm
           | ANF.AVar vname when is_toplevel vname = None ->
             emit ld t1 (Addr_of_local.pp_to_mach vname);
             emit sd t1 (ROffset (a0, i * wordsize ()))
           | x ->
             helper_a (DReg "t0") x;
             emit ld a0 (Addr_of_local.pp_to_mach block_addr);
             emit
               sd
               t0
               (ROffset (a0, i * wordsize ()))
               ~comm:(sprintf "setting field %d" i);
             ())
        fields;
      emit ld t0 (Addr_of_local.pp_to_mach block_addr);
      emit sd_dest t0 dest;
      emit shrink_stack 2)
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

let prepare_string_lit_init ppf anf =
  let () =
    let iter =
      { ANF.default_iterator with
        aconst =
          (fun _ -> function
             | Parsetree.PConst_string s -> String_lit_hash.extend s string_list_hash
             | _ -> ())
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
  if String_lit_hash.is_empty string_list_hash
  then fun () -> ()
  else (
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
        emit sd a0 (ROffset (Temp_reg 1, 0))))
;;

let emit_global_constant is_toplevel ppf ident expr =
  printfn ppf "";
  printfn ppf ".data # global %a" Toplevel.pp_label_exn ident;
  printfn ppf ".align 3";
  printfn ppf "%a: .quad 0x0" Toplevel.pp_label_exn ident;
  printfn ppf ".text";
  printfn ppf ".globl .init_%a" Toplevel.pp_label_exn ident;
  printfn ppf "init_%a:" Toplevel.pp_label_exn ident;
  (* printfn ppf "  push rbp"; *)
  (* printfn ppf "  mov rbp, rsp"; *)
  generate_body is_toplevel expr;
  emit lla t1 (Format.asprintf "%a" Toplevel.pp_label_exn ident);
  emit sd a0 (ROffset (Temp_reg 1, 0));
  (* emit addi sp sp (-16); *)
  (* emit sd ra (ROffset (SP, 0)); *)
  emit comment "call rukaml_add_gc_static_root";
  (* emit ld ra (ROffset (SP, 0)); *)
  (* emit shrink_stack 2; *)
  emit ret;
  Machine.flush_queue ppf
;;

(* printfn ppf "  mov qword [rel %a], rax" Toplevel.pp_label_exn ident;
  printfn ppf "  lea rdi, [rel %a]" Toplevel.pp_label_exn ident;
  printfn ppf "  call add_gc_static_root";
  printfn ppf "  pop rbp";
  printfn ppf "  ret ;;; init_%a" Toplevel.pp_label_exn ident;
  Machine.flush_queue ppf *)

let put_init_global_immediates ppf =
  printfn ppf "";
  printfn ppf ".text";
  printfn ppf ".globl rukaml_init_global_immediates";
  printfn ppf "rukaml_init_global_immediates:";
  emit grow_stack 2;
  emit sd ra (ROffset (SP, 0));
  (* printfn ppf "  push rbp"; *)
  (* printfn ppf "  mov rbp, rsp"; *)
  Toplevel.iter_immediates (fun { ident; _ } ->
    (* printfn ppf "  call init_%a" Toplevel.pp_label_exn ident *)
    emit call (Format.asprintf "init_%a" Toplevel.pp_label_exn ident));
  (* printfn ppf "  pop rbp"; *)
  (* printfn ppf "  ret ;;; rukaml_init_global_immediates"; *)
  emit ld ra (ROffset (SP, 0));
  emit shrink_stack 2;
  emit ret;
  Machine.flush_queue ppf
;;

let emit_global_eval ppf is_toplevel ident expr =
  printfn ppf "\n.text";
  printfn ppf "init_%a:" Toplevel.pp_label_exn ident;
  (* emit addi sp sp (-16); *)
  (* emit sd ra (ROffset (SP, 0)); *)
  generate_body is_toplevel expr;
  (* emit ld ra (ROffset (SP, 0)); *)
  (* emit shrink_stack 2 *)
  emit ret;
  Machine.flush_queue ppf
;;

let codegen ?(wrap_main_into_start = true) anf file =
  let _ = wrap_main_into_start in
  Stdio.Out_channel.with_file file ~f:(fun ch ->
    let ppf = Format.formatter_of_out_channel ch in
    let is_toplevel, vbs =
      let hash = Hashtbl.create (List.length anf) in
      let is_toplevel =
        fun name ->
        match Hashtbl.find hash name with
        | n -> Some n
        | exception Not_found -> None
      in
      let vbs =
        List.map
          (function
            | _, None, body ->
              let fresh = ANF.gensym_id ~prefix:"eval" () in
              Toplevel.extend fresh ~kind:(Immediate Eval);
              `Global_eval (fresh, body)
            | _, Some name, body ->
              let pats, expr = Compile_lib.ANF.group_abstractions body in
              let argc = List.length pats in
              if String.equal name.Ident.hum_name "main"
              then (
                let () = Toplevel.extend name ~kind:Toplevel.Main in
                `Main (name, body))
              else if argc = 0
              then (
                let () = Toplevel.extend name ~kind:Toplevel.(Immediate Constant) in
                (* Format.eprintf "Toplvel: add immediate '%a'\n%!" Ident.pp name; *)
                `Immediate (name, body))
              else (
                let () = assert (argc >= 1 || name.Ident.hum_name = "main") in
                let () = Toplevel.extend name ~kind:(Toplevel.Function { argc }) in
                Hashtbl.add hash name argc;
                `Function (name, pats, expr)))
          anf
      in
      is_toplevel, vbs
    in
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
    let open Compile_lib in
    let on_vb = function
      | `Function (name, pats, body) ->
        let argc = List.length pats in
        assert (argc > 0);
        printfn ppf "\n.text";
        let () = printfn ppf ".globl %s" name.Ident.hum_name in
        let () = printfn ppf "%s:" name.Ident.hum_name in
        let names =
          List.filter_map
            (function
              | ANF.Apat_var name -> Some name
              | Apat_unit -> None
              | p -> failwiths "not implemented: pattern %a" ANF.pp_apat p)
            pats
        in
        List.rev pats
        |> ListLabels.iteri ~f:(fun i -> function
          | ANF.Apat_var name -> Addr_of_local.add_arg ~argc i name
          | Apat_unit -> ()
          | _ -> failwith "not implemented");
        generate_body is_toplevel body;
        Addr_of_local.remove_args names;
        print_epilogue ppf name.hum_name;
        Machine.flush_queue ppf
      | `Main (name, expr) ->
        put_init_global_immediates ppf;
        printfn ppf "\n.text";
        printfn ppf ".globl main";
        printfn ppf "main:";
        emit mv a2 a1 ~comm:"argv";
        emit mv a1 a0 ~comm:"argc";
        emit mv a0 sp ~comm:"Stack pointer is first";
        emit call "rukaml_initialize";
        do_string_init ();
        emit call "rukaml_init_global_immediates";
        emit comment "this is main";
        emit li a0 0;
        Toplevel.extend name ~kind:Main;
        generate_body is_toplevel expr;
        print_epilogue ppf name.Ident.hum_name;
        Machine.flush_queue ppf
      | `Immediate (name, expr) -> emit_global_constant is_toplevel ppf name expr
      | `Global_eval (name, expr) -> emit_global_eval ppf is_toplevel name expr
      (* | { kind = Alias _ } -> failwith "TODO alias" *)
      (* | _ -> failwith "TODO" *)
    in
    List.iter on_vb vbs;
    Format.pp_print_flush ppf ());
  Result.Ok ()
;;
