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
   4) arguments go to the stack from right to the left (unexpected order)
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
  printfn ppf "section .text";
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
  (* rukaml_discard is reserved word in .bss where result of vb with '_' pattern is discarded *)
  | DDiscard
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

(* int stands for formal arity *)
let stdlib_externs =
  [ 1, "add_gc_static_root"
  ; 2, "rukaml_alloc_closure"
  ; 1, "rukaml_print_int"
  ; 7, "rukaml_print_int_kaml"
  ; 2, "rukaml_applyN"
  ; 2, "rukaml_field"
  ; 1, "rukaml_tag"
  ; 1, "rukaml_size"
  ; 2, "rukaml_alloc_pair"
  ; 2, "rukaml_alloc_block"
  ; 0, "rukaml_array_stdin"
  ; 8, "rukaml_array_set"
  ; 6, "rukaml_array_read_in"
  ; 7, "rukaml_block_size"
  ; 7, "rukaml_block_tag"
  ; 8, "rukaml_block_nth"
  ; 0, "rukaml_match_failure"
  ; 3, "rukaml_initialize"
  ; 1, "rukaml_gc_compact"
  ; 1, "rukaml_gc_print_stats"
  ; 1, "rukaml_print_alloc_closure_count"
  ; 7, "rukaml_alloc_printf_closure"
  ; 8, "rukaml_alloc_fprintf_closure"
  ; 7, "rukaml_alloc_sprintf_closure"
  ; 0, "rukaml_stdout"
  ; 2, "rukaml_equal_struct"
  ; 7, "rukaml_string_of_char_list"
  ; 8, "rukaml_string_equal"
  ; 8, "rukaml_string_nth"
  ; 7, "rukaml_string_len"
  ; 2, "rukaml_apply1"
  ; 3, "rukaml_apply2"
  ; 0, "rukaml_stdin"
  ; 0, "rukaml_stdout"
  ; 0, "rukaml_stderr"
  ; 7, "rukaml_open_in"
  ; 7, "rukaml_open_out"
  ; 7, "rukaml_close_channel"
  ; 7, "rukaml_end_of_input"
  ; 7, "rukaml_input_char"
  ; 0, "rukaml_argv"
  ]
;;

let stdlib_aliases =
  [ "print", "rukaml_print_int_kaml"
  ; "printf", "rukaml_alloc_printf_closure"
  ; "fprintf", "rukaml_alloc_fprintf_closure"
  ; "sprintf", "rukaml_alloc_sprintf_closure"
  ; "string_len", "rukaml_string_len"
  ; "string_nth", "rukaml_string_nth"
  ; "string_equal", "rukaml_string_equal"
  ; "string_of_char_list", "rukaml_string_of_char_list"
  ; "array_len", "rukaml_block_size"
  ; "array_get", "rukaml_block_nth"
  ; "array_set", "rukaml_array_set"
  ; "block_nth", "rukaml_block_nth"
  ; "block_size", "rukaml_block_size"
  ; "block_tag", "rukaml_block_tag"
  ; "stdin", "rukaml_stdin"
  ; "stdout", "rukaml_stdout"
  ; "stderr", "rukaml_stderr"
  ; "end_of_input", "rukaml_end_of_input"
  ; "input_char", "rukaml_input_char"
  ; "gc_stats", "rukaml_gc_print_stats"
  ; "gc_compact", "rukaml_gc_compact"
  ; "closure_count", "rukaml_print_alloc_closure_count"
  ; "open_in", "rukaml_open_in"
  ; "open_out", "rukaml_open_out"
  ; "close_in", "rukaml_close_channel"
  ; "close_out", "rukaml_close_channel"
  ; "sys_argv", "rukaml_argv"
  ]
;;

module Toplevel = struct
  (* immediate is anything that needs to be evaluated before control flow enters main *)
  type immediate =
    | Constant (* let x = <expr which is not a lambda> *)
    | Match (* let 42 = <expr> *)
    | Eval (* let () = <expr> or let _ = <expr> *)

  type kind =
    | Main (* main: *)
    | Alias of { aliasee : Ident.t }
    | Function of { argc : int }
    | Immediate of immediate

  type t =
    { ident : Ident.t
    ; kind : kind
    }

  type toplevel = t

  let store : (Ident.t, toplevel) Hashtbl.t = Hashtbl.create 100
  let contains (ident : Ident.t) = Hashtbl.mem store ident
  let has_key = contains
  let is_toplevel = contains

  let rec find_opt (ident : Ident.t) =
    match Hashtbl.find_opt store ident with
    | Some { kind = Alias { aliasee }; _ } -> find_opt aliasee
    | x -> x
  ;;

  let rec find_exn (ident : Ident.t) =
    match Hashtbl.find_opt store ident with
    | Some { kind = Alias { aliasee }; _ } -> find_exn aliasee
    | Some x -> x
    | None ->
      Format.eprintf "Can't find toplevel %a" Ident.pp ident;
      raise Not_found
  ;;

  (* __immediates is used to make toplevel evaluation in the order of declaration *)
  let __immediates : toplevel Queue.t = Queue.create ()
  let iter_immediates f = Queue.iter f __immediates

  let extend (ident : Ident.t) ~kind =
    let toplevel = { ident; kind } in
    Hashtbl.add store ident toplevel;
    match kind with
    | Immediate _ -> Queue.add toplevel __immediates
    | _ -> ()
  ;;

  let rec pp_toplevel_ident ppf { ident; kind } =
    match kind with
    | Main -> Format.fprintf ppf "main"
    | Alias { aliasee } -> pp_toplevel_ident ppf (find_exn aliasee)
    | _ -> Ident.pp ppf ident
  ;;

  let pp_label_exn ppf (ident : Ident.t) =
    let toplevel = find_exn ident in
    pp_toplevel_ident ppf toplevel
  ;;

  let pp_toplevel_exn ppf (ident : Ident.t) =
    let toplevel = find_exn ident in
    Format.fprintf ppf "[%a]" pp_toplevel_ident toplevel
  ;;

  let is_toplevel_function (ident : Ident.t) =
    match find_opt ident with
    | Some { kind = Function _; _ } -> true
    | _ -> false
  ;;

  let is_toplevel_constant (ident : Ident.t) =
    match find_opt ident with
    | Some { kind = Immediate Constant; _ } -> true
    | _ -> false
  ;;

  let is_main (ident : Ident.t) =
    match find_opt ident with
    | Some { kind = Main; _ } -> true
    | _ -> false
  ;;

  (* TODO: it is not the best way to resolve aliases *)
  let rec resolve_alias fident =
    match find_exn fident with
    | { kind = Alias { aliasee }; _ } -> resolve_alias aliasee
    | { ident; _ } -> ident
  ;;
end

module Mangling = struct
  let bounded =
    (* main, stdlib_externs and aliases for them are initially bounded identifiers *)
    [ Ident.ident "main" 0 ]
    @ List.map (fun (_argc, ident) -> Ident.ident ident 0) stdlib_externs
    @ List.map (fun (alias, _aliasee) -> Ident.ident alias 0) stdlib_aliases
  ;;

  let mangle_names_stru = Compile_lib.Mangling.mangle_names_stru ~bounded
end

module Addr_of_var = struct
  let pp_var_exn ppf ident =
    match Addr_of_local.pp_local_exn ppf ident with
    | () -> log "Found local variable %s" ident.hum_name
    | exception Not_found ->
      (match Toplevel.pp_toplevel_exn ppf ident with
       | () -> log "Found global variable %s" ident.hum_name
       | exception Not_found ->
         failwiths "Can't find location of a variable %s" ident.hum_name)
  ;;

  let is_defined (ident : Ident.t) = Addr_of_local.has_key ident || Toplevel.has_key ident
end

let pp_dest ppf = function
  | DDiscard -> fprintf ppf "[rukaml_discard]"
  | DReg s -> fprintf ppf "%s" s
  | DStack_var name -> Addr_of_local.pp_local_exn ppf name
;;

let emit_alloc_closure ppf ~fname ~argc =
  printfn ppf "  mov rdi, %a" Toplevel.pp_label_exn fname;
  printfn ppf "  mov rsi, %d" argc;
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
    | ELet _ -> assert false
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
      | AConst (PConst_char c) -> pp_access ~doc:"constant" (Char.code c)
      | AConst (PConst_string s) ->
        (* notice: strings representation differs from adt/array/tuple, so rukaml_emit_alloc_block is not used here *)
        let payload_words_n = (String.length s + 7) / 8 in
        (* +1 to store String.length s in the last word *)
        printfn ppf "  mov rdi, %d ; size of block for string" (payload_words_n + 1);
        printfn ppf "  mov rsi, 252 ; string tag";
        printfn ppf "  call rukaml_alloc_block";
        List.iteri
          (fun i ch -> printfn ppf "  mov qword [rax+%d], %d" i (Char.code ch))
          (Base.String.to_list s);
        printfn ppf "  mov qword [rax+8*%d], %d" payload_words_n (String.length s);
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | APrimitive ("stdin", 0) ->
        printfn ppf "  call rukaml_stdin";
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | APrimitive ("stdout", 0) ->
        printfn ppf "  call rukaml_stdout";
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | APrimitive ("stderr", 0) ->
        printfn ppf "  call rukaml_stderr";
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "get_arg"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_constructor_arg") 2;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "get"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_array_get") 2;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "set"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_array_set") 3;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "stdin"; _ } ->
        printfn ppf "  call rukaml_array_stdin";
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
      | APrimitive ("print", (1 as parity)) ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_print_int_kaml") parity;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AConstruct _ -> assert false
      | APrimitive _ as arg -> failwiths "Primitive %a is not supported" ANF.pp_a arg
      | ATuple _ -> assert false
      | AArray _ -> assert false
      | _ -> failwith "not implemented");
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
    | ELet _ -> assert false
  and helper_c (dest : dest) = function
    | CIte (CAtom (AConst (Frontend.Parsetree.PConst_bool true)), bth, _bel) ->
      helper dest bth
    | CIte (CAtom (AConst (Frontend.Parsetree.PConst_bool false)), _bth, bel) ->
      helper dest bel
    | CIte
        ( CApp
            ( APrimitive ("=", 2)
            , AConst (Frontend.Parsetree.PConst_int l)
            , [ AConst (Frontend.Parsetree.PConst_int r) ] )
        , bth
        , bel ) ->
      (* This is not entirely correct, because OCaml number and target could be different *)
      helper dest (if l = r then bth else bel)
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
    | CApp (APrimitive ("exit", 1), AVar arg, []) ->
      printfn ppf "  mov rdi, %a" Addr_of_var.pp_var_exn arg;
      printfn ppf "  mov rax, 60 ; syscall exit";
      printfn ppf "  syscall"
    | CApp (APrimitive ("exit", 1), AConst (PConst_int n), []) ->
      printfn ppf "  mov rdi, %d" n;
      printfn ppf "  mov rax, 60 ; syscall exit";
      printfn ppf "  syscall"
    | CApp (APrimitive ("print", 1), AVar arg, []) ->
      printfn ppf "  mov rdi, %a" Addr_of_local.pp_local_exn arg;
      printfn ppf "  call rukaml_print_int";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive ("block_tag", 1), obj, []) ->
      helper_a (DReg "rsi") obj;
      printfn ppf "  mov rdi, rukaml_block_tag";
      printfn ppf "  call rukaml_apply1";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive ("block_size", 1), obj, []) ->
      helper_a (DReg "rsi") obj;
      printfn ppf "  mov rdi, rukaml_block_size";
      printfn ppf "  call rukaml_apply1";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive ("field", 2), AConst (PConst_int n), [ obj ])
    | CApp (APrimitive ("block_nth", 2), obj, [ AConst (PConst_int n) ]) ->
      helper_a (DReg "rsi") obj;
      printfn ppf "  mov rdi, rukaml_block_nth";
      printfn ppf "  mov rdx, %d" n;
      printfn ppf "  call rukaml_apply2";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive ("field", 2), AVar v, [ obj ])
    | CApp (APrimitive ("block_nth", 2), obj, [ AVar v ])
      when Addr_of_var.is_defined v ->
      helper_a (DReg "rsi") obj;
      printfn ppf "  mov rdi, rukaml_block_nth";
      printfn ppf "  mov rdx, %a" Addr_of_var.pp_var_exn v;
      printfn ppf "  call rukaml_apply2";
      printfn ppf "  mov %a, rax" pp_dest dest
    (* >>> TODO: get rid of it *)
    | CApp (APrimitive (("fprintf" as fname), (2 as argc)), arg1, []) ->
      emit_rukaml_applyN dest ~fname ~arg1 ~argc
    | CApp (APrimitive (("block_nth" as fname), (2 as argc)), arg1, []) ->
      emit_rukaml_applyN dest ~fname ~arg1 ~argc
    | CApp (APrimitive (("string_nth" as fname), (2 as argc)), arg1, []) ->
      emit_rukaml_applyN dest ~fname ~arg1 ~argc
    | CApp (APrimitive (("string_equal" as fname), (2 as argc)), arg1, []) ->
      emit_rukaml_applyN dest ~fname ~arg1 ~argc
    | CApp (APrimitive (("array_get" as fname), (2 as argc)), arg1, []) ->
      emit_rukaml_applyN dest ~fname ~arg1 ~argc
    (* <<< *)
    | CApp (APrimitive (("printf" as fname), 1), arg, []) ->
      emit_rukaml_apply1 dest ~fname ~arg
    | CApp (APrimitive (("sprintf" as fname), 1), arg, []) ->
      emit_rukaml_apply1 dest ~fname ~arg
    | CApp (APrimitive (("fprintf" as fname), 2), arg1, [ arg2 ]) ->
      emit_rukaml_apply2 dest ~fname ~arg1 ~arg2
    | CApp (APrimitive (("string_len" as fname), 1), arg, []) ->
      emit_rukaml_apply1 dest ~fname ~arg
    | CApp (APrimitive (("block_nth" as fname), 2), arg1, [ arg2 ]) ->
      emit_rukaml_apply2 dest ~fname ~arg1 ~arg2
    | CApp (APrimitive (("string_nth" as fname), 2), arg1, [ arg2 ]) ->
      emit_rukaml_apply2 dest ~fname ~arg1 ~arg2
    | CApp (APrimitive (("string_equal" as fname), 2), arg1, [ arg2 ]) ->
      emit_rukaml_apply2 dest ~fname ~arg1 ~arg2
    | CApp (APrimitive (("string_of_char_list" as fname), 1), arg, []) ->
      emit_rukaml_apply1 dest ~arg ~fname
    | CApp (APrimitive (("open_in" as fname), 1), arg, []) ->
      emit_rukaml_apply1 dest ~arg ~fname
    | CApp (APrimitive (("open_out" as fname), 1), arg, []) ->
      emit_rukaml_apply1 dest ~arg ~fname
    | CApp (APrimitive (("input_char" as fname), 1), arg, []) ->
      emit_rukaml_apply1 dest ~arg ~fname
    | CApp (APrimitive (("end_of_input" as fname), 1), arg, []) ->
      emit_rukaml_apply1 dest ~arg ~fname
    | CApp (APrimitive ((("close_in" | "close_out") as fname), 1), arg, []) ->
      emit_rukaml_apply1 dest ~fname ~arg
    | CApp (APrimitive (("array_set" as fname), (3 as argc)), arg1, []) ->
      emit_rukaml_applyN dest ~fname ~argc ~arg1
    | CApp (APrimitive (("array_get" as fname), 2), arg1, [ arg2 ]) ->
      emit_rukaml_apply2 dest ~fname ~arg1 ~arg2
    | CApp (APrimitive (("array_len" as fname), 1), arg, []) ->
      emit_rukaml_apply1 dest ~fname ~arg
    | CApp (APrimitive ("char_code", 1), arg1, []) ->
      (match arg1 with
       | AVar v when Addr_of_local.has_key v ->
         printfn ppf "  mov r11, %a" Addr_of_local.pp_local_exn v;
         printfn ppf "  mov %a, r11" pp_dest dest
       | AConst (PConst_char c) ->
         printfn ppf "  mov qword %a, %d" pp_dest dest (Char.code c)
       | _ -> failwith "Should not happen")
    | CApp (APrimitive ("=", 2), AConst (PConst_int l), [ AConst (PConst_int r) ]) ->
      if l = r
      then printfn ppf "  mov qword %a, 1" pp_dest dest
      else printfn ppf "  mov qword %a, 0" pp_dest dest
    | CApp (APrimitive ("=", 2), AConst (PConst_int n), [ AVar vname ])
    | CApp (APrimitive ("=", 2), AVar vname, [ AConst (PConst_int n) ]) ->
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
    | CApp (APrimitive ("=", 2), AVar v1, [ AVar v2 ])
      when Addr_of_var.(is_defined v1 && is_defined v2) ->
      printfn ppf "  mov qword rdi, %a" Addr_of_var.pp_var_exn v1;
      printfn ppf "  mov qword rsi, %a" Addr_of_var.pp_var_exn v2;
      printfn ppf "  call rukaml_equal_struct";
      printfn ppf "  mov qword %a, rax" pp_dest dest
    | CApp (APrimitive ("=", 2), AVar var, [ obj ]) when Addr_of_var.is_defined var ->
      helper_a (DReg "rdi") obj;
      printfn ppf "  mov qword rsi, %a" Addr_of_var.pp_var_exn var;
      printfn ppf "  call rukaml_equal_struct";
      printfn ppf "  mov qword %a, rax" pp_dest dest
    | CApp (APrimitive ("=", 2), obj, [ AVar var ]) when Addr_of_var.is_defined var ->
      helper_a (DReg "rdi") obj;
      printfn ppf "  mov qword rsi, %a" Addr_of_var.pp_var_exn var;
      printfn ppf "  call rukaml_equal_struct";
      printfn ppf "  mov qword %a, rax" pp_dest dest
    | CApp (APrimitive ("=", 2), a1, [ a2 ]) ->
      helper_a (DReg "rdi") a1;
      let name1 = Ident.of_string @@ gen_name ~prefix:"pad" () in
      let name2 = Ident.of_string @@ gen_name ~prefix:"arg1" () in
      Addr_of_local.extend name1;
      Addr_of_local.extend name2;
      printfn ppf "  add rsp, -8*2";
      printfn ppf "  mov qword [rsp], rdi";
      helper_a (DReg "rsi") a2;
      printfn ppf "  mov qword rdi, [rsp]";
      printfn ppf "  add rsp, 8*2";
      Addr_of_local.remove_local name2;
      Addr_of_local.remove_local name1;
      printfn ppf "  call rukaml_equal_struct";
      printfn ppf "  mov qword %a, rax" pp_dest dest
    | CApp (APrimitive ("+", 2), AVar vname, [ AConst (PConst_int n) ])
    | CApp (APrimitive ("+", 2), AConst (PConst_int n), [ AVar vname ]) ->
      printfn ppf "  mov qword r11, %a" Addr_of_var.pp_var_exn vname;
      if n = 1 then printfn ppf "  inc r11" else printfn ppf "  add r11, %d" n;
      printfn ppf "  mov qword %a, r11" pp_dest dest
    | CApp (APrimitive ("*", 2), AVar vname, [ AConst (PConst_int n) ])
    | CApp (APrimitive ("*", 2), AConst (PConst_int n), [ AVar vname ]) ->
      printfn ppf "  mov qword r11, %a" Addr_of_var.pp_var_exn vname;
      printfn ppf "  imul r11, %d" n;
      printfn ppf "  mov qword %a, r11" pp_dest dest
    | CApp (APrimitive ("-", 2), AVar vname, [ AConst (PConst_int n) ]) ->
      printfn ppf "  mov qword r11, %a" Addr_of_var.pp_var_exn vname;
      if n = 1 then printfn ppf "  dec r11" else printfn ppf "  sub r11, %d" n;
      printfn ppf "  mov qword %a, r11" pp_dest dest
    | CApp (APrimitive ("-", 2), AConst (PConst_int n), [ AVar vname ]) ->
      printfn ppf "  mov qword r11, %d" n;
      printfn ppf "  mov qword r12, %a" Addr_of_var.pp_var_exn vname;
      printfn ppf "  sub r11, r12";
      printfn ppf "  mov qword %a, r11" pp_dest dest
    (* TODO?: make folding optional here *)
    | CApp
        ( APrimitive ((("+" | "*" | "-") as op), 2)
        , AConst (PConst_int n1)
        , [ AConst (PConst_int n2) ] ) ->
      let n =
        match op with
        | "+" -> n1 + n2
        | "*" -> n1 * n2
        | "-" -> n1 - n2
        | _ -> assert false
      in
      printfn ppf "  mov qword %a, %d" pp_dest dest n
    (* TODO?: make folding optional here *)
    | CApp
        ( APrimitive ((("&&" | "||") as op), 2)
        , AConst (PConst_bool b1)
        , [ AConst (PConst_bool b2) ] ) ->
      let n =
        match op with
        | "&&" -> Bool.to_int (b1 && b2)
        | "||" -> Bool.to_int (b1 || b2)
        | _ -> assert false
      in
      printfn ppf "  mov qword %a, %d" pp_dest dest n
    | CApp (APrimitive ((("+" | "*" | "-" | "&&" | "||") as op), 2), AVar vl, [ AVar vr ])
      ->
      printfn ppf "  mov qword r11, %a" Addr_of_var.pp_var_exn vl;
      printfn ppf "  mov qword r12, %a" Addr_of_var.pp_var_exn vr;
      printfn
        ppf
        "  %s r11, r12"
        (match op with
         | "+" -> "add"
         | "*" -> "imul"
         | "-" -> "sub"
         | "&&" -> "and"
         | "||" -> "or"
         | _ -> assert false);
      printfn ppf "  mov %a, r11" pp_dest dest
    | CApp (APrimitive ((("<" | ">" | "<=" | ">=") as op), 2), l, [ r ]) ->
      (match l, r with
       | AConst (PConst_int n), AVar v ->
         printfn ppf "  mov qword r11, %d" n;
         printfn ppf "  mov qword r12, %a" Addr_of_var.pp_var_exn v
       | AVar v, AConst (PConst_int n) ->
         printfn ppf "  mov qword r11, %a" Addr_of_var.pp_var_exn v;
         printfn ppf "  mov qword r12, %d" n
       | AConst (PConst_char ch), AVar v ->
         printfn ppf "  mov qword r11, %d" (Char.code ch);
         printfn ppf "  mov qword r12, %a" Addr_of_var.pp_var_exn v
       | AVar v, AConst (PConst_char ch) ->
         printfn ppf "  mov qword r11, %a" Addr_of_var.pp_var_exn v;
         printfn ppf "  mov qword r12, %d" (Char.code ch)
       | AVar vl, AVar vr ->
         printfn ppf "  mov qword r11, %a" Addr_of_var.pp_var_exn vl;
         printfn ppf "  mov qword r12, %a" Addr_of_var.pp_var_exn vr
       (* TODO: it can be folded *)
       | AConst (PConst_int n1), AConst (PConst_int n2) ->
         printfn ppf "  mov qword r11, %d" n1;
         printfn ppf "  mov qword r12, %d" n2
       | AConst (PConst_char ch1), AConst (PConst_char ch2) ->
         printfn ppf "  mov qword r11, %d" (Char.code ch1);
         printfn ppf "  mov qword r12, %d" (Char.code ch2)
       | _ -> assert false);
      printfn ppf "  cmp qword r11, r12";
      printfn
        ppf
        (match op with
         | "<" -> "setl r11b"
         | ">" -> "setg r11b"
         | "<=" -> "setle r11b"
         | ">=" -> "setge r11b"
         | _ -> assert false);
      printfn ppf "  and r11, 1";
      printfn ppf "mov qword %a, r11" pp_dest dest
    | CApp (APrimitive ("&&", 2), AVar v, [ AConst (PConst_bool true) ])
    | CApp (APrimitive ("&&", 2), AConst (PConst_bool true), [ AVar v ])
    | CApp (APrimitive ("||", 2), AVar v, [ AConst (PConst_bool false) ])
    | CApp (APrimitive ("||", 2), AConst (PConst_bool false), [ AVar v ]) ->
      printfn ppf "  mov qword r11, %a" Addr_of_var.pp_var_exn v;
      printfn ppf "  mov %a, r11" pp_dest dest
    | CApp (APrimitive ("||", 2), AConst (PConst_bool true), [ _ ])
    | CApp (APrimitive ("||", 2), _, [ AConst (PConst_bool true) ]) ->
      printfn ppf "  mov %a, 1" pp_dest dest
    | CApp (APrimitive ("&&", 2), AConst (PConst_bool false), [ _ ])
    | CApp (APrimitive ("&&", 2), _, [ AConst (PConst_bool false) ]) ->
      printfn ppf "  mov %a, 0" pp_dest dest
    | CApp (AVar f, arg1, []) when Toplevel.is_toplevel_constant f ->
      (* f is closure *)
      helper_a (DReg "rdx") arg1;
      printfn ppf "  mov rax, 0  ; no float arguments";
      printfn ppf "  mov rdi, [%a]" Toplevel.pp_label_exn f;
      printfn ppf "  mov rsi, 1 ; argc";
      printfn ppf "  call rukaml_applyN";
      printfn ppf "  mov %a, rax" pp_dest dest
    (* TODO: | CApp (AVar f, (AUnit as arg), []) *)
    | CApp (APrimitive ("print", 1), AConst (PConst_int n), []) ->
      printfn ppf "  mov rdi, %d" n;
      printfn ppf "  call rukaml_print_int";
      printfn ppf "  mov qword %a, 0" pp_dest dest
    | CApp (APrimitive ("gc_compact", 1), _, []) ->
      printfn ppf "  mov rdi, rsp";
      printfn ppf "  mov rsi, 0";
      printfn ppf "  call rukaml_gc_compact"
    | CApp (APrimitive ("gc_stats", 1), _, []) ->
      printfn ppf "  mov rdi, 0";
      printfn ppf "  mov rsi, 0";
      printfn ppf "  call rukaml_gc_print_stats"
    | CApp (APrimitive ("closure_count", 1), _, []) ->
      printfn ppf "  mov rdi, 0";
      printfn ppf "  mov rsi, 0";
      printfn ppf "  call rukaml_print_alloc_closure_count"
    | CApp (AVar f, arg1, args) as _cexpr when Toplevel.is_toplevel_function f ->
      (* Callig a rukaml function uses custom calling convention.
           CDECL convention: all arguments on stack, LTR *)
      let expected_arity =
        match Toplevel.find_exn f with
        | { kind = Function { argc }; _ } -> argc
        | _ -> assert false
      in
      let formal_arity = 1 + List.length args in
      (* printfn
        ppf
        "\t; expected_arity = %d\n\t; formal_arity = %d"
        expected_arity
        formal_arity; *)
      (* printfn ppf "@[; calling @[%a@]@]" ANF.pp_c cexpr; *)
      if expected_arity = formal_arity
      then (
        let to_remove = allocate_args (arg1 :: args) in
        printfn ppf "  call %a" Toplevel.pp_label_exn f;
        printfn ppf "  add rsp, 8*%d ; dealloc args" to_remove;
        printfn ppf "  mov %a, rax" pp_dest dest)
      else if formal_arity < expected_arity
      then (
        let partial_args_count = allocate_args (arg1 :: args) in
        printfn ppf "  mov rdi, %a" Toplevel.pp_label_exn f;
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
    | CApp (AVar f, (AConst _ as arg), [])
    | CApp (AVar f, (APrimitive _ as arg), [])
    | CApp (AVar f, (AVar _ as arg), []) ->
      assert (Option.is_none (is_toplevel f));
      let arg =
        match arg with
        (* TODO(Kakadu): change to builtin *)
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
    | CApp (APrimitive ("field", 2), AConst (PConst_int n), [ (AVar _ as cont) ]) ->
      (* TODO(Kakadu): field vs get_arg? *)
      helper_a (DReg "rsi") cont;
      printfn ppf "  mov rdi, %d" n;
      printfn ppf "  call rukaml_field";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive ("print", 1), AConst (PConst_int n), []) ->
      printfn ppf "  mov rdi, %d" n;
      printfn ppf "  call rukaml_print_int";
      printfn ppf "  mov qword %a, 0" pp_dest dest
    | CApp (APrimitive ("get_arg", 2), AConst (PConst_int n), [ constr ]) ->
      (match constr with
       | AVar v when Addr_of_local.has_key v ->
         let name1 = Ident.of_string @@ gen_name ~prefix:"pad" () in
         let name2 = Ident.of_string @@ gen_name ~prefix:"constr" () in
         Addr_of_local.extend name1;
         Addr_of_local.extend name2;
         printfn ppf "  add rsp, -8*2";
         printfn ppf "  mov r11, %a" Addr_of_local.pp_local_exn v;
         printfn ppf "  mov qword [rsp], %d" n;
         printfn ppf "  mov qword [rsp+8], r11";
         printfn ppf "  call rukaml_constructor_arg";
         printfn ppf "  mov %a, rax" pp_dest dest;
         printfn ppf "  add rsp, 8*2";
         Addr_of_local.remove_local name2;
         Addr_of_local.remove_local name1
       | _ -> failwith "Should not happen")
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
      Format.eprintf "@[`%s`@]\n%!" (Compile_lib.ANF.show_c_expr anf);
      failwiths "Not implemented %d" __LINE__
    | rest ->
      printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__;
      printfn ppf "; @[<h>%a@]" Compile_lib.ANF.pp_c rest
  and helper_a (dest : dest) x =
    (* log "  %s: dest=`%a`, expr = %a" __FUNCTION__ pp_dest dest ANF.pp_a x; *)
    match x with
    | AConst (Frontend.Parsetree.PConst_bool true) ->
      printfn ppf "  mov qword %a, 1" pp_dest dest
    | AConst (Frontend.Parsetree.PConst_bool false) | AUnit ->
      printfn ppf "  mov qword %a, 0" pp_dest dest
    | AConst (Frontend.Parsetree.PConst_int n) ->
      printfn ppf "  mov qword %a,  %d" pp_dest dest n
    | AConst (Frontend.Parsetree.PConst_char c) ->
      printfn ppf "  mov qword %a,  %d" pp_dest dest (Char.code c)
    | APrimitive ("print", 1) ->
      emit_alloc_closure ppf ~fname:(Ident.ident "rukaml_print_int_kaml" 0) ~argc:1;
      printfn ppf "  mov %a, rax" pp_dest dest
    | APrimitive ("stdin", 0) ->
      printfn ppf "  call rukaml_stdin";
      printfn ppf "  mov %a, rax" pp_dest dest
    | APrimitive ("stdout", 0) ->
      printfn ppf "  call rukaml_stdout";
      printfn ppf "  mov %a, rax" pp_dest dest
    | APrimitive ("stderr", 0) ->
      printfn ppf "  call rukaml_stderr";
      printfn ppf "  mov %a, rax" pp_dest dest
    | APrimitive ("sys_argv", 0) ->
      printfn ppf "  call rukaml_argv";
      printfn ppf "  mov %a, rax" pp_dest dest
    | AVar vname when Addr_of_local.has_key vname ->
      printfn
        ppf
        "  mov qword rdx, %a ; access local var \"%a\" [1/2]"
        Addr_of_local.pp_local_exn
        vname
        Ident.pp
        vname;
      printfn
        ppf
        "  mov qword %a, rdx  ; access local var \"%a\" [2/2]"
        pp_dest
        dest
        Ident.pp
        vname
    | AVar vname when Toplevel.is_toplevel vname ->
      (match Toplevel.find_exn vname with
       | { kind = Function { argc = 0 }; ident } ->
         (* TODO: it's weird *)
         printfn ppf "  call %a" Ident.pp ident;
         printfn ppf "  mov %a, rax" pp_dest dest
       | { kind = Function { argc }; ident = fname } ->
         assert (argc > 0);
         emit_alloc_closure ppf ~fname ~argc;
         printfn ppf "  mov %a, rax" pp_dest dest
       | { kind = Immediate Constant; ident } ->
         printfn ppf "  mov rax, [%a]" Toplevel.pp_label_exn ident;
         printfn ppf "  mov qword %a, rax" pp_dest dest
       | { kind = Main; _ } -> assert false
       | { kind = Alias _; _ } -> assert false
       | { kind = Immediate Eval; _ } -> assert false
       | { kind = Immediate Match; _ } -> assert false)
    | AConstruct (tag, []) -> printfn ppf "  mov qword %a, %d" pp_dest dest tag
    | AConstruct (tag, fields) -> emit_initialize_block dest ~fields ~tag ~name:"adt"
    | ATuple (x1, x2, xs) ->
      emit_initialize_block dest ~fields:(x1 :: x2 :: xs) ~tag:0 ~name:"tuple"
    | AArray fields -> emit_initialize_block dest ~fields ~tag:1 ~name:"array"
    | APrimitive ("match_failure", _) -> printfn ppf "  call rukaml_match_failure"
    | AConst (PConst_string s) ->
      (* notice: DO NOT use emit_initialize_block here. strings representation differs *)
      let payload_words_n = (String.length s + 7) / 8 in
      (* +1 to store String.length s in the last word *)
      printfn ppf "  mov rdi, %d ; size of block for string" (payload_words_n + 1);
      printfn ppf "  mov rsi, 252 ; string tag";
      printfn ppf "  call rukaml_alloc_block";
      List.iteri
        (fun i ch -> printfn ppf "  mov qword [rax+%d], %d" i (Char.code ch))
        (Base.String.to_list s);
      printfn ppf "  mov qword [rax+8*%d], %d" payload_words_n (String.length s);
      printfn ppf "  mov %a, rax" pp_dest dest
    | ALam (pat, body) ->
      let pats, body = ANF.group_abstractions body in
      let pats = pat :: pats in
      let argc = List.length pats in
      let names = List.map (fun (ANF.Apat_var name) -> name) pats in
      let lam_name = Ident.of_string (Printf.sprintf "__lam_%d" (gensym ())) in
      Toplevel.extend lam_name ~kind:(Function { argc });
      printfn ppf "section .text";
      printfn ppf "GLOBAL %a" Toplevel.pp_label_exn lam_name;
      printfn ppf "@[<h>%a:@]" Toplevel.pp_label_exn lam_name;
      List.rev names
      |> ListLabels.iteri ~f:(fun i name -> Addr_of_local.add_arg ~argc i name);
      printfn ppf "  push rbp";
      printfn ppf "  mov  rbp, rsp";
      generate_body ppf body;
      Addr_of_local.remove_args names;
      print_epilogue ppf (Format.asprintf "%a" Toplevel.pp_label_exn lam_name);
      emit_alloc_closure ppf ~fname:lam_name ~argc;
      printfn ppf "  mov %a, rax" pp_dest dest
    | APrimitive ("closure_count", (1 as argc)) ->
      emit_alloc_closure
        ppf
        ~fname:(Ident.ident "rukaml_print_alloc_closure_count" 0)
        ~argc;
      printfn ppf "  mov %a, rax" pp_dest dest
    | atom ->
      printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__;
      failwiths "Unsupported: %a" ANF.pp_a atom
  and emit_initialize_block dest ~tag ~fields ~name =
    printfn ppf "  mov rdi, %d ; %s size" (List.length fields) name;
    printfn ppf "  mov rsi, %d ; %s tag" tag name;
    printfn ppf "  call rukaml_alloc_block";
    let name1 = Ident.of_string @@ gen_name ~prefix:"pad" () in
    let name2 = Ident.of_string @@ gen_name ~prefix:name () in
    Addr_of_local.extend name1;
    Addr_of_local.extend name2;
    printfn ppf "  add rsp, -8*2 ; push %s" name;
    printfn ppf "  mov qword [rsp], rax";
    List.iteri
      (fun i x ->
         helper_a (DReg "rsi") x;
         printfn ppf "  mov qword rdi, [rsp] ; get %s" name;
         printfn ppf "  mov qword [rdi+8*%d], rsi ; set field of %s" i name)
      fields;
    printfn ppf "  mov rax, [rsp]";
    printfn ppf "  add rsp, 8*2 ; pop %s" name;
    Addr_of_local.remove_local name2;
    Addr_of_local.remove_local name1;
    printfn ppf "  mov %a, rax" pp_dest dest
  and emit_rukaml_apply1 dest ~fname ~arg =
    let fident = Ident.ident fname 0 in
    match arg with
    | ANF.AVar v when Addr_of_var.is_defined v ->
      printfn ppf "  mov rdi, %a" Toplevel.pp_label_exn fident;
      printfn ppf "  mov rsi, %a" Addr_of_var.pp_var_exn v;
      printfn ppf "  call rukaml_apply1";
      printfn ppf "  mov %a, rax" pp_dest dest
    | _ ->
      helper_a (DReg "rsi") arg;
      printfn ppf "  mov rdi, %a" Toplevel.pp_label_exn fident;
      printfn ppf "  call rukaml_apply1";
      printfn ppf "  mov %a, rax" pp_dest dest
  and emit_rukaml_apply2 dest ~fname ~arg1 ~arg2 =
    let fident = Ident.ident fname 0 in
    (match arg1, arg2 with
     | ANF.AVar v1, ANF.AVar v2 ->
       printfn ppf "  mov rsi, %a" Addr_of_var.pp_var_exn v1;
       printfn ppf "  mov rdx, %a" Addr_of_var.pp_var_exn v2
     | ANF.AVar v1, _ ->
       helper_a (DReg "rdx") arg2;
       printfn ppf "  mov rsi, %a" Addr_of_var.pp_var_exn v1
     | _, ANF.AVar v2 ->
       helper_a (DReg "rsi") arg1;
       printfn ppf "  mov rdx, %a" Addr_of_var.pp_var_exn v2
     | _ ->
       helper_a (DReg "rsi") arg1;
       printfn ppf "  add rsp, -8*2";
       printfn ppf "  mov qword [rsp], rsi";
       helper_a (DReg "rdx") arg2;
       printfn ppf "  mov qword rsi, [rsp]";
       printfn ppf "  add rsp, 8*2");
    printfn ppf "  mov rdi, %a" Toplevel.pp_label_exn fident;
    printfn ppf "  call rukaml_apply1";
    printfn ppf "  mov %a, rax" pp_dest dest
  and emit_rukaml_applyN dest ~fname ~argc ~arg1 =
    let fident = Ident.ident fname 0 in
    assert (argc > 1);
    match arg1 with
    | ANF.AVar v when Addr_of_var.is_defined v ->
      printfn ppf "  mov rdi, %a" Toplevel.pp_label_exn fident;
      printfn ppf "  mov rsi, %d" argc;
      printfn ppf "  call rukaml_alloc_closure";
      printfn ppf "  mov rdi, rax";
      printfn ppf "  mov rsi, 1";
      printfn ppf "  mov rdx, %a" Addr_of_var.pp_var_exn v;
      printfn ppf "  mov rax, 0";
      printfn ppf "  call rukaml_applyN";
      printfn ppf "  mov %a, rax" pp_dest dest
    | _ ->
      printfn ppf "  mov rdi, %a" Toplevel.pp_label_exn fident;
      printfn ppf "  mov rsi, %d" argc;
      printfn ppf "  call rukaml_alloc_closure";
      let name1 = Ident.of_string @@ gen_name ~prefix:"pad" () in
      let name2 = Ident.of_string @@ gen_name ~prefix:"arg1" () in
      Addr_of_local.extend name1;
      Addr_of_local.extend name2;
      printfn ppf "  add rsp, -8*2 ; closure";
      printfn ppf "  mov [rsp], rax";
      helper_a (DReg "rdx") arg1;
      printfn ppf "  mov rdi, [rsp]";
      printfn ppf "  add rsp, 8*2 ; closure";
      Addr_of_local.remove_local name2;
      Addr_of_local.remove_local name1;
      printfn ppf "  mov rsi, 1";
      printfn ppf "  mov rax, 0";
      printfn ppf "  call rukaml_applyN";
      printfn ppf "  mov %a, rax" pp_dest dest
  in
  let dealloc_locals = allocate_locals ppf body in
  helper (DReg "rax") body;
  dealloc_locals ~now:()
;;

let put_print_newline ppf =
  printfn
    ppf
    {|
    section .text

    print_newline:
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
section .text

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

let put_init_stdlib ppf =
  List.iter
    (fun (argc, name) ->
       Toplevel.extend (Ident.ident name 0) ~kind:(Function { argc });
       printfn ppf "extern %s" name)
    stdlib_externs;
  List.iter
    (fun (alias, _aliasee) ->
       Toplevel.extend
         (Ident.ident alias 0)
         ~kind:(Alias { aliasee = Ident.ident _aliasee 0 }))
    stdlib_aliases
;;

let put_init_global_immediates ppf =
  printfn ppf "section .text";
  printfn ppf "rukaml_init_global_immediates:";
  printfn ppf "  push rbp";
  printfn ppf "  mov rbp, rsp";
  Toplevel.iter_immediates (fun { ident; _ } ->
    printfn ppf "  call init_%a" Toplevel.pp_label_exn ident);
  printfn ppf "  pop rbp";
  printfn ppf "  ret ;;; rukaml_init_global_immediates"
;;

(* TODO: may be it is useless and rax should be used for DDiscard *)
let put_discard ppf =
  printfn ppf "section .bss";
  printfn ppf "  rukaml_discard: resq 1"
;;

(* let <name> = ... *)
let emit_global_constant ppf ident expr =
  printfn ppf "section .bss";
  printfn ppf "  global %a" Toplevel.pp_label_exn ident;
  printfn ppf "  %a:    resq 1" Toplevel.pp_label_exn ident;
  printfn ppf "section .text";
  printfn ppf "init_%a:" Toplevel.pp_label_exn ident;
  printfn ppf "  push rbp";
  printfn ppf "  mov rbp, rsp";
  generate_body ppf expr;
  printfn ppf "  mov qword [%a], rax" Toplevel.pp_label_exn ident;
  printfn ppf "  lea rdi, [%a]" Toplevel.pp_label_exn ident;
  printfn ppf "  call add_gc_static_root";
  printfn ppf "  pop rbp";
  printfn ppf "  ret ;;; init_%a" Toplevel.pp_label_exn ident
;;

(* let () = ... or let _ = ... *)
let emit_global_eval ppf ident expr =
  printfn ppf "section .text";
  printfn ppf "init_%a:" Toplevel.pp_label_exn ident;
  printfn ppf "  push rbp";
  printfn ppf "  mov rbp, rsp";
  generate_body ppf expr;
  printfn ppf "  mov qword %a, rax" pp_dest DDiscard;
  printfn ppf "  pop rbp";
  printfn ppf "  ret ;;; init_%a" Toplevel.pp_label_exn ident
;;

(* let <constant> = ... *)
let emit_global_match ppf ident const expr =
  printfn ppf "section .text";
  printfn ppf "init_%a:" Toplevel.pp_label_exn ident;
  printfn ppf "  push rbp";
  printfn ppf "  mov rbp, rsp";
  printfn ppf "  ; begin eval matching lhs";
  (* result in rax *)
  generate_body ppf ANF.(EComplex (CAtom (AConst const)));
  printfn ppf "  ; end eval matching rhs";
  (*
     TODO: for now some assert fails with this one
  let name1 = Ident.of_string @@ gen_name ~prefix:"pad" () in
  let name2 = Ident.of_string @@ gen_name ~prefix:"lhs" () in
  Addr_of_local.extend name1;
  Addr_of_local.extend name2; *)
  printfn ppf "  add rsp, -8*2 ; matching lhs";
  printfn ppf "  mov qword [rsp], rax";
  printfn ppf "  ; begin eval matching rhs";
  (* result in rax *)
  generate_body ppf expr;
  printfn ppf "  ; end eval matching rhs";
  printfn ppf "  mov qword rdi, rax";
  printfn ppf "  mov qword rsi, [rsp]";
  printfn ppf "  call rukaml_equal_struct ; matching result";
  printfn ppf "  cmp rax, 0";
  (* TODO? : make call here *)
  printfn ppf "  je rukaml_match_failure";
  printfn ppf "  add rsp, 8*2 ; matching lhs";
  (* Addr_of_local.remove_local name2;
  Addr_of_local.remove_local name1; *)
  printfn ppf "  pop rbp";
  printfn ppf "  ret ;;; init_%a" Toplevel.pp_label_exn ident
;;

let codegen ?(wrap_main_into_start = true) anf file =
  let anf = Mangling.mangle_names_stru anf in
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
    if use_custom_main
    then (
      put_print_newline ppf;
      put_print_hex ppf);
    printfn ppf "section .text";
    put_init_stdlib ppf;
    printfn ppf "";
    put_discard ppf;
    printfn ppf "";
    if use_custom_main
    then
      (* TODO: use exit_group syscall (231)
           https://filippo.io/linux-syscall-table/ *)
      printfn
        ppf
        {|
section .text
    _start:
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
        {|
        section .text
        _start:
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
