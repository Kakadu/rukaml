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
  (* rukaml_discard is reserved word in .bss where garbage of '_' pattern is discarded. i think it makes logic of generated code clearer than placing the result into rax *)
  | DDiscard
  | DReg of string
  | DStack_var of Frontend.Ident.t
    (* DStatic_var's are allocated in .bss and access to them is performed via labels *)
  | DStatic_var of Frontend.Ident.t

module Addr_of_local = struct
  let store : (Frontend.Ident.t, _) Hashtbl.t = Hashtbl.create 100
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
  let find_exn name = Hashtbl.find store name
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

(* warning:
    DO NOT access toplevel constants and toplevel functions using Ident.pp
    use Toplevel.pp_label_exn instead of it
    global labels should be unique and this module provides it *)
module Toplevel = struct
  type kind =
    | Main (* main: *)
    | Extern (* labels defined in stdlib_externs *) of { argc : int }
    | Alias (* stdlib_aliases *) of { aliasee : string }
    | Constant (* .bss labels *)
    | Function (* .text labels (except main) *) of { arity : int }

  type t =
    { ident : Ident.t
    ; kind : kind
    }

  type toplevel = t

  (* 1. it DOES NOT use Ident.t as keys to evaluate hashes properly (it does not care about id, only hum_name matters)
     2. it IS NOT persistent (which is ok for one module program but I'm not sure about several ones) *)
  let store : (string, toplevel) Hashtbl.t = Hashtbl.create 100
  let contains (ident : Ident.t) = Hashtbl.mem store ident.hum_name
  let has_key = contains
  let is_toplevel = contains
  let find_opt (ident : Ident.t) = Hashtbl.find_opt store ident.hum_name

  let rec find_exn (ident : Ident.t) =
    match Hashtbl.find_opt store ident.hum_name with
    | Some { kind = Alias { aliasee }; _ } -> find_exn (Ident.ident aliasee 0)
    | Some x -> x
    | None -> failwiths "not found: %a" Ident.pp ident
  ;;

  (* it is important to evaluate constants in initialization (in rukaml_init_constants) in order of declaration, so it stores them separately *)
  let __constants : Ident.t Queue.t = Queue.create ()
  let iter_constants f = Queue.iter f __constants

  let extend (ident : Ident.t) ~kind =
    Hashtbl.add store ident.hum_name { ident; kind };
    if kind = Constant then Queue.add ident __constants
  ;;

  let pp_toplevel_label ppf { ident; kind } =
    match kind with
    | Alias { aliasee } -> Format.fprintf ppf "%s" aliasee
    | Extern _ | Main -> Format.fprintf ppf "%s" ident.hum_name
    | Constant -> Format.fprintf ppf "%s__0%d" ident.hum_name ident.id
    | Function _ -> Format.fprintf ppf "%s__1%d" ident.hum_name ident.id
  ;;

  let pp_label_exn ppf (ident : Ident.t) =
    let toplevel = find_exn ident in
    pp_toplevel_label ppf toplevel
  ;;

  let pp_toplevel_exn ppf (ident : Ident.t) =
    let toplevel = find_exn ident in
    Format.fprintf ppf "[%a]" pp_toplevel_label toplevel
  ;;

  let is_toplevel_function (ident : Ident.t) =
    match find_opt ident with
    | Some { kind = Function _; _ } -> true
    | _ -> false
  ;;

  let is_toplevel_constant (ident : Ident.t) =
    match find_opt ident with
    | Some { kind = Constant; _ } -> true
    | _ -> false
  ;;

  let is_main (ident : Ident.t) =
    match find_opt ident with
    | Some { kind = Main; _ } -> true
    | _ -> false
  ;;
end

module Addr_of_var = struct
  (* TODO:
        Addr_of_var looks for
            1. local variables
            2. global labels (.bss, .text, externs and externs' aliases)
        In some cases we need to check only local ones using Addr_of_local.
        I'm not sure if it is truly correct for now. *)
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

  let is_builtin (ident : Ident.t) =
    not
      (Addr_of_local.has_key ident
       ||
       match Toplevel.find_opt ident with
       | Some { kind = Constant | Function _; _ } -> true
       | _ -> false)
  ;;
end

let pp_dest ppf = function
  | DDiscard -> fprintf ppf "[rukaml_discard]"
  | DReg s -> fprintf ppf "%s" s
  | DStack_var name -> Addr_of_local.pp_local_exn ppf name
  | DStatic_var name -> Toplevel.pp_toplevel_exn ppf name
;;

let emit_alloc_closure ppf ident ~arity =
  printfn ppf "  mov rdi, %a" Toplevel.pp_label_exn ident;
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
    | ELet (_flg, (Tpat_unit | Tpat_any), rhs, where_) ->
      helper_c rhs;
      helper where_
    | ELet (_, p, _, _) ->
      failwiths "TODO: not implemented patt { %a }" Frontend.Pprinttyped.pp_pattern p
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

let rec generate_body ppf body =
  let is_toplevel = Toplevel.is_toplevel in
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
        Format.fprintf ppf "  mov qword [rsp%+d*8], %d" (count - 1 - i) v;
        if doc <> "" then printfn ppf " ; %s" doc else printfn ppf ""
      in
      function
      | Compile_lib.ANF.AUnit | AConst (PConst_bool false) -> pp_access 0
      | AConst (PConst_bool true) -> pp_access 1
      | AConst (PConst_int n) -> pp_access ~doc:"constant" n
      | AConst (PConst_char c) -> pp_access ~doc:"constant" (Char.code c)
      | AConst (PConst_string s) ->
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
      | AVar vname when is_toplevel vname ->
        (match Toplevel.find_exn vname with
         | { kind = Function { arity }; _ } ->
           emit_alloc_closure ppf vname ~arity;
           printfn
             ppf
             "  mov qword [rsp%+d*8], rax ; arg \"%a\""
             (count - 1 - i)
             Toplevel.pp_toplevel_exn
             vname
         | { kind = Constant; _ } ->
           printfn ppf "  mov rax, %a" Toplevel.pp_toplevel_exn vname;
           printfn ppf "  mov qword [rsp+%d*8], rax" (count - 1 - i)
         | { kind = Extern { argc }; _ } ->
           emit_alloc_closure ppf vname ~arity:argc;
           printfn ppf "  mov qword [rsp+%d*8], rax" (count - 1 - i)
         | _ -> failwiths "TODO: should not happen ???")
      | AVar { Ident.hum_name = "print"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_print_int_kaml") ~arity:1;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "open_in"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_array_read_in") ~arity:1;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "block_size"; _ }
      | AVar { Ident.hum_name = "array_len"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_block_size") ~arity:1;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "block_tag"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_block_tag") ~arity:1;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "block_nth"; _ }
      | AVar { Ident.hum_name = "array_get"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_block_nth") ~arity:2;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "array_set"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_array_set") ~arity:3;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "stdin"; _ } ->
        printfn ppf "  call rukaml_array_stdin";
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "stdout"; _ } ->
        printfn ppf "  call rukaml_stdout";
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "printf"; _ } ->
        (* 1 stands for fmt here *)
        emit_alloc_closure ppf (Ident.of_string "rukaml_printf_closure") ~arity:1;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "sprintf"; _ } ->
        (* 1 stands for fmt here *)
        emit_alloc_closure ppf (Ident.of_string "rukaml_sprintf_closure") ~arity:1;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "string_of_char_list"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_string_of_char_list") ~arity:1;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "string_equal"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_string_equal") ~arity:2;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "string_len"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_string_len") ~arity:1;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "string_nth"; _ } ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_string_nth") ~arity:2;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar { Ident.hum_name = "fprintf"; _ } ->
        (* 2 stands for out_channel and fmt here *)
        emit_alloc_closure ppf (Ident.of_string "rukaml_fprintf_closure") ~arity:2;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AVar vname ->
        printfn
          ppf
          "  mov qword r8, %a  ; arg \"%a\""
          Addr_of_var.pp_var_exn
          vname
          Ident.pp
          vname;
        printfn ppf "  mov qword [rsp%+d*8], r8" (count - 1 - i)
      | ALam _ -> failwith "Should it be representable in ANF?"
      | APrimitive ("print", (1 as arity)) ->
        emit_alloc_closure ppf (Ident.of_string "rukaml_print_int_kaml") ~arity;
        printfn ppf "  mov qword [rsp%+d*8], rax" (count - 1 - i)
      | AConstruct _ -> assert false
      | APrimitive _ -> assert false
      | ATuple _ -> assert false
      | AArray _ -> assert false);
    count + _stack_padding
  in
  let rec helper dest = function
    | Compile_lib.ANF.EComplex c -> helper_c dest c
    | ELet (_, Tpat_var name, rhs, wher) ->
      assert (Addr_of_local.contains name);
      let rhs_dest = DStack_var name in
      (* printfn ppf "    ;; calculate rhs and put into %a. offset = %d" pp_dest
           dest
           (Addr_of_local.find_exn name); *)
      helper_c rhs_dest rhs;
      helper dest wher
    | ELet (_, (Tpat_any | Tpat_unit), rhs, wher) ->
      helper_c DDiscard rhs;
      helper dest wher
    | ELet _ -> failwiths "TODO: implement pattern matching here"
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
    | CIte (CAtom (AVar econd), bth, bel) ->
      printfn ppf "  mov qword rdx, %a" Addr_of_var.pp_var_exn econd;
      printfn ppf "  cmp rdx, 0";
      let el_lab = Printf.sprintf "lab_then_%d" (gensym ()) in
      let fin_lab = Printf.sprintf "lab_endif_%d" (gensym ()) in
      printfn ppf "  je %s" el_lab;
      helper dest bth;
      printfn ppf "  jmp %s" fin_lab;
      printfn ppf "%s:" el_lab;
      helper dest bel;
      printfn ppf "%s:" fin_lab
    | CAtom (AVar f)
    (* TODO(Kakadu): change to builtin *)
      when f.Ident.hum_name = "stdin" && Addr_of_var.is_builtin f ->
      printfn ppf "  call rukaml_array_stdin";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CAtom (AVar f)
    (* TODO(Kakadu): change to builtin *)
      when f.Ident.hum_name = "stdout" && Addr_of_var.is_builtin f ->
      printfn ppf "  call rukaml_stdout";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive ("print", 1), AVar arg, []) ->
      printfn ppf "  mov rdi, %a" Addr_of_var.pp_var_exn arg;
      printfn ppf "  call rukaml_print_int";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive ("block_tag", _), obj, []) ->
      (* TODO? : it is not cdecl but emits less code *)
      helper_a (DReg "rdi") obj;
      printfn ppf "  call rukaml_block_tag_imm";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive ("block_size", _), obj, []) ->
      (* TODO? : it is not cdecl but emits less code *)
      helper_a (DReg "rdi") obj;
      printfn ppf "  call rukaml_block_size_imm";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive ("block_nth", _), obj, [ AConst (PConst_int n) ]) ->
      (* TODO? : it is not cdecl but emits less code *)
      helper_a (DReg "rdi") obj;
      printfn ppf "  mov rsi, %d" n;
      printfn ppf "  call rukaml_block_nth_imm";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive ("block_nth", _), obj, [ AVar v ]) when Addr_of_var.is_defined v ->
      (* TODO? : it is not cdecl but emits less code *)
      helper_a (DReg "rdi") obj;
      printfn ppf "  mov rsi, %a" Addr_of_var.pp_var_exn v;
      printfn ppf "  call rukaml_block_nth_imm";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (AVar f, arg, []) when f.Ident.hum_name = "printf" && Addr_of_var.is_builtin f
      ->
      helper_a (DReg "r11") arg;
      printfn ppf "  add rsp, -8*2 ; space and padding for fmt";
      printfn ppf "  mov qword [rsp], r11 ; push fmt";
      printfn ppf "  call rukaml_alloc_printf_closure";
      printfn ppf "  add rsp, 8*2 ; space and padding for fmt";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (AVar f, arg, []) when f.Ident.hum_name = "sprintf" && Addr_of_var.is_builtin f
      ->
      helper_a (DReg "r11") arg;
      printfn ppf "  add rsp, -8*2 ; space and padding for fmt";
      printfn ppf "  mov qword [rsp], r11 ; push fmt";
      printfn ppf "  call rukaml_alloc_sprintf_closure";
      printfn ppf "  add rsp, 8*2 ; space and padding for fmt";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (AVar f, arg1, [])
      when f.Ident.hum_name = "fprintf" && Addr_of_var.is_builtin f ->
      printfn ppf "  mov rdi, rukaml_alloc_fprintf_closure";
      printfn ppf "  mov rsi, 2";
      printfn ppf "  call rukaml_alloc_closure";
      printfn ppf "  add rsp, -8*2";
      printfn ppf "  mov [rsp], rax";
      helper_a (DReg "rdx") arg1;
      printfn ppf "  mov rdi, [rsp]";
      printfn ppf "  add rsp, 8*2";
      printfn ppf "  mov rsi, 1";
      printfn ppf "  mov al, 0";
      printfn ppf "  call rukaml_applyN";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (AVar (ident : Ident.t), AVar v, [])
    (* TODO? : it is not cdecl but emits less code *)
      when ident.hum_name = "string_len" && Addr_of_var.is_builtin ident ->
      printfn ppf "  mov rdi, rukaml_string_len";
      printfn ppf "  mov rsi, %a" Addr_of_var.pp_var_exn v;
      printfn ppf "  call rukaml_apply1";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (AVar (ident : Ident.t), arg, [])
    (* TODO? : it is not cdecl but emits less code *)
      when ident.hum_name = "string_len" && Addr_of_var.is_builtin ident ->
      helper_a (DReg "rsi") arg;
      printfn ppf "  mov rdi, rukaml_string_len";
      printfn ppf "  call rukaml_apply1";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (AVar (ident : Ident.t), AVar v, [])
    (* TODO? : it is not cdecl but emits less code *)
      when (ident.hum_name = "rukaml_block_size"
            || ident.hum_name = "array_len"
            || ident.hum_name = "get_arity")
           && Addr_of_var.is_builtin ident ->
      printfn ppf "  mov rdi, rukaml_block_size";
      printfn ppf "  mov rsi, %a" Addr_of_var.pp_var_exn v;
      printfn ppf "  call rukaml_apply1";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (AVar (ident : Ident.t), arg, [])
    (* TODO? : it is not cdecl but emits less code *)
      when (ident.hum_name = "rukaml_block_size"
            || ident.hum_name = "array_len"
            || ident.hum_name = "get_arity")
           && Addr_of_var.is_builtin ident ->
      helper_a (DReg "rsi") arg;
      printfn ppf "  mov rdi, rukaml_block_size";
      printfn ppf "  call rukaml_apply1";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive ("field", 2), AConst (PConst_int n), [ AVar v ])
    (* TODO? : it is not cdecl but emits less code *)
      when Addr_of_var.is_defined v ->
      printfn ppf "  mov rdi, %a" Addr_of_var.pp_var_exn v;
      printfn ppf "  mov rsi, %d" n;
      printfn ppf "  call rukaml_block_nth_imm";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (AVar (ident : Ident.t), arg, [])
      when Addr_of_var.is_builtin ident && ident.hum_name = "string_nth" ->
      helper_a (DReg "r11") arg;
      printfn ppf "  add rsp, -8*2";
      printfn ppf "  mov qword [rsp], r11";
      printfn ppf "  mov rdi, rukaml_string_nth";
      printfn ppf "  mov rsi, 2";
      printfn ppf "  call rukaml_alloc_closure";
      printfn ppf "  mov rdi, rax";
      printfn ppf "  mov rsi, 1";
      printfn ppf "  mov rdx, [rsp]";
      printfn ppf "  mov al, 0";
      printfn ppf "  call rukaml_applyN";
      printfn ppf "  add rsp, 8*2";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (AVar f, arg1, [])
      when f.Ident.hum_name = "string_equal" && Addr_of_var.is_builtin f ->
      printfn ppf "  mov rdi, rukaml_string_equal";
      printfn ppf "  mov rsi, 2";
      printfn ppf "  call rukaml_alloc_closure";
      printfn ppf "  add rsp, -8*2";
      printfn ppf "  mov [rsp], rax";
      helper_a (DReg "rdx") arg1;
      printfn ppf "  mov rdi, [rsp]";
      printfn ppf "  add rsp, 8*2";
      printfn ppf "  mov rsi, 1";
      printfn ppf "  mov al, 0";
      printfn ppf "  call rukaml_applyN";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (AVar f, arg, [])
      when f.Ident.hum_name = "string_of_char_list" && Addr_of_var.is_builtin f ->
      helper_a (DReg "rsi") arg;
      printfn ppf "  mov rdi, rukaml_string_of_char_list";
      printfn ppf "  call rukaml_apply1";
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (AVar f, arg1, [])
    (* TODO(Kakadu): change to builtin *)
      when f.Ident.hum_name = "array_set" && Addr_of_var.is_builtin f ->
      (match arg1 with
       | AVar arr when Addr_of_var.is_defined arr ->
         printfn ppf "  mov rdi, rukaml_array_set";
         printfn ppf "  mov rsi, 3";
         printfn ppf "  call rukaml_alloc_closure";
         printfn ppf "  mov rdi, rax";
         printfn ppf "  mov rsi, 1";
         printfn ppf "  mov rdx, %a" Addr_of_var.pp_var_exn arr;
         printfn ppf "  mov al, 0";
         printfn ppf "  call rukaml_applyN";
         printfn ppf "  mov %a, rax" pp_dest dest
       | _ -> failwith "Should not happen")
    | CApp (APrimitive ("char_code", 1), arg1, []) ->
      (match arg1 with
       | AVar v ->
         printfn ppf "  mov r11, %a" Addr_of_var.pp_var_exn v;
         printfn ppf "  mov %a, r11" pp_dest dest
       | AConst (PConst_char c) ->
         printfn ppf "  mov qword %a, %d" pp_dest dest (Char.code c)
       | _ -> failwith "Should not happen")
    | CApp (AVar f, arg1, [])
    (* TODO(Kakadu): change to builtin *)
      when f.Ident.hum_name = "open_in" && Addr_of_var.is_builtin f ->
      (match arg1 with
       | AVar v when Addr_of_var.is_defined v ->
         let name1 = Ident.of_string @@ gen_name ~prefix:"pad" () in
         let name2 = Ident.of_string @@ gen_name ~prefix:"open_in arg" () in
         Addr_of_local.extend name1;
         Addr_of_local.extend name2;
         printfn ppf "  add rsp, -8*2";
         printfn ppf "  mov r11, %a" Addr_of_var.pp_var_exn v;
         printfn ppf "  mov qword [rsp], r11";
         printfn ppf "  call rukaml_array_read_in";
         printfn ppf "  mov %a, rax" pp_dest dest;
         printfn ppf "  add rsp, 8*2";
         Addr_of_local.remove_local name2;
         Addr_of_local.remove_local name1
       | AArray _ ->
         helper_a (DReg "r11") arg1;
         printfn ppf "  mov qword [rsp], r11";
         printfn ppf "  call rukaml_array_read_in";
         printfn ppf "  mov %a, rax" pp_dest dest
       | _ -> failwith "Should not happen")
    | CApp (AVar f, arg1, [])
      when f.Ident.hum_name = "open_out" && Addr_of_var.is_builtin f ->
      (match arg1 with
       | AVar v when Addr_of_var.is_defined v ->
         let name1 = Ident.of_string @@ gen_name ~prefix:"pad" () in
         let name2 = Ident.of_string @@ gen_name ~prefix:"open_out arg" () in
         Addr_of_local.extend name1;
         Addr_of_local.extend name2;
         printfn ppf "  add rsp, -8*2";
         printfn ppf "  mov r11, %a" Addr_of_var.pp_var_exn v;
         printfn ppf "  mov qword [rsp], r11";
         printfn ppf "  call rukaml_open_out";
         printfn ppf "  mov %a, rax" pp_dest dest;
         printfn ppf "  add rsp, 8*2";
         Addr_of_local.remove_local name2;
         Addr_of_local.remove_local name1
       | AConst (PConst_string _) ->
         helper_a (DReg "r11") arg1;
         printfn ppf "  mov qword [rsp], r11";
         printfn ppf "  call rukaml_open_out";
         printfn ppf "  mov %a, rax" pp_dest dest
       | _ -> failwith "Should not happen")
    | CApp (AVar f, arg1, [])
    (* TODO(Kakadu): change to builtin *)
      when f.Ident.hum_name = "print" && Addr_of_var.is_builtin f ->
      (match arg1 with
       | AVar v ->
         let name1 = Ident.of_string @@ gen_name ~prefix:"pad" () in
         let name2 = Ident.of_string @@ gen_name ~prefix:"print_arg" () in
         Addr_of_local.extend name1;
         Addr_of_local.extend name2;
         printfn ppf "  add rsp, -8*2";
         printfn ppf "  mov r11, %a" Addr_of_var.pp_var_exn v;
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
         printfn ppf "  mov qword %a, %d" Addr_of_var.pp_var_exn name2 n;
         printfn ppf "  call rukaml_print_int";
         printfn ppf "  add rsp, 8*2";
         Addr_of_local.remove_local name2;
         Addr_of_local.remove_local name1;
         printfn ppf "  mov %a, rax" pp_dest dest
       | _ -> failwith "Should not happen")
    | CApp (APrimitive ("=", 2), AConst (PConst_int l), [ AConst (PConst_int r) ]) ->
      if l = r
      then printfn ppf "  mov qword %a, 1" pp_dest dest
      else printfn ppf "  mov qword %a, 0" pp_dest dest
    | CApp (APrimitive ("=", 2), AConst (PConst_int n), [ AVar vname ])
    | CApp (APrimitive ("=", 2), AVar vname, [ AConst (PConst_int n) ]) ->
      printfn ppf "  mov qword r11, %a" Addr_of_var.pp_var_exn vname;
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
    | CApp (APrimitive ("=", 2), AVar v1, [ AVar v2 ]) ->
      (* TODO? : it is not cdecl but emits less code *)
      printfn ppf "  mov qword rdi, %a" Addr_of_var.pp_var_exn v1;
      printfn ppf "  mov qword rsi, %a" Addr_of_var.pp_var_exn v2;
      printfn ppf "  call rukaml_equal_struct";
      printfn ppf "  mov qword %a, rax" pp_dest dest
    | CApp (APrimitive ("=", 2), a1, [ a2 ]) ->
      (* TODO? : it is not cdecl but emits less code *)
      helper_a (DReg "r11") a1;
      printfn ppf "  push r11";
      helper_a (DReg "rsi") a2;
      printfn ppf "  pop rdi";
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
       (* TODO: it can be folded (in separated case) *)
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
    | CApp (AVar f, arg1, args) as _cexpr when Toplevel.is_toplevel_function f ->
      (* Callig a rukaml function uses custom calling convention.
           CDECL convention: all arguments on stack, LTR *)
      let expected_arity =
        match Toplevel.find_exn f with
        | { kind = Function { arity }; _ } -> arity
        | _ -> assert false
      in
      let formal_arity = 1 + List.length args in
      (* printfn
        ppf
        "\t; expected_arity = %d\n\t; formal_arity = %d"
        expected_arity
        formal_arity; *)
      (* printfn ppf "@[; calling @[%a@]@]" ANF.pp_c _cexpr; *)
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
        (* printfn ppf "  add rsp, -8*2 ; deallocate closure value and padding" *))
      else failwith "Arity mismatch: over application"
    | CApp (AVar f, (AConst _ as arg), [])
    | CApp (AVar f, (APrimitive _ as arg), [])
    | CApp (AVar f, (AVar _ as arg), []) ->
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
      (* "f" may global constans, so it uses pp_var_exn *)
      printfn ppf "  mov rdi, %a" Addr_of_var.pp_var_exn f;
      printfn ppf "  mov rsi, 1";
      (* arg1 is local variable, so it uses pp_local_exn (but pp_var_exn would work too) *)
      printfn ppf "  mov rdx, %a" Addr_of_local.pp_local_exn arg1;
      printfn ppf "  call rukaml_applyN";
      Addr_of_local.remove_local arg1;
      Addr_of_local.remove_local temp_padding;
      printfn ppf "  add rsp, 8*2 ; free space for args of function \"%a\"" Ident.pp f;
      printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive ("print", 1), AConst (PConst_int n), []) ->
      printfn ppf "  mov rdi, %d" n;
      printfn ppf "  call rukaml_print_int";
      printfn ppf "  mov qword %a, 0" pp_dest dest
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
    | AVar ({ Ident.hum_name = "print"; _ } as v) when Addr_of_var.is_builtin v ->
      emit_alloc_closure ppf (Ident.of_string "rukaml_print_int") ~arity:1;
      printfn ppf "  mov %a, rax" pp_dest dest
    | AVar ({ Ident.hum_name = "stdout"; _ } as v) when Addr_of_var.is_builtin v ->
      printfn ppf "  call rukaml_stdout";
      printfn ppf "  mov %a, rax" pp_dest dest
    | AVar ({ Ident.hum_name = "stderr"; _ } as v) when Addr_of_var.is_builtin v ->
      printfn ppf "  call rukaml_stderr";
      printfn ppf "  mov %a, rax" pp_dest dest
    | AVar ({ Ident.hum_name = "rukaml_block_size" | "get_arity" | "array_len"; _ } as v)
      when Addr_of_var.is_builtin v ->
      emit_alloc_closure ppf (Ident.of_string "rukaml_block_size") ~arity:1;
      printfn ppf "  mov %a, rax" pp_dest dest
    | AVar ({ Ident.hum_name = "rukaml_string_len"; _ } as v)
      when Addr_of_var.is_builtin v ->
      emit_alloc_closure ppf (Ident.of_string "rukaml_string_len") ~arity:1;
      printfn ppf "  mov %a, rax" pp_dest dest
    | AVar ({ Ident.hum_name = "printf"; _ } as v) when Addr_of_var.is_builtin v ->
      emit_alloc_closure ppf (Ident.of_string "rukaml_alloc_printf_closure") ~arity:1;
      printfn ppf "  mov %a, rax" pp_dest dest
    | AVar ({ Ident.hum_name = "fprintf"; _ } as v) when Addr_of_var.is_builtin v ->
      emit_alloc_closure ppf (Ident.of_string "rukaml_alloc_fprintf_closure") ~arity:2;
      printfn ppf "  mov %a, rax" pp_dest dest
    | AVar ({ Ident.hum_name = "sprintf"; _ } as v) when Addr_of_var.is_builtin v ->
      emit_alloc_closure ppf (Ident.of_string "rukaml_alloc_sprintf_closure") ~arity:1;
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
       | { kind = Function { arity }; _ } ->
         emit_alloc_closure ppf vname ~arity;
         printfn ppf "  mov %a, rax" pp_dest dest
       | { kind = Constant; _ } ->
         printfn ppf "  mov rax, %a" Toplevel.pp_toplevel_exn vname;
         printfn ppf "  mov qword %a, rax" pp_dest dest
       | _ -> failwiths "should it happen ???")
    | AConstruct (tag, args) ->
      printfn ppf "  mov rdi, %d ; adt variant arity" (List.length args);
      printfn ppf "  mov rsi, %d ; adt variant tag" tag;
      printfn ppf "  call rukaml_alloc_block";
      List.iteri
        (fun i x ->
           (* TODO: rewrite using stack once *)
           printfn ppf "  add rsp, -8*2";
           printfn ppf "  mov qword [rsp], rax";
           helper_a (DReg "rdi") x;
           printfn ppf "  mov rax, [rsp]";
           printfn ppf "  add rsp, 8*2";
           printfn ppf "  mov qword [rax+8*%d], rdi" i)
        args;
      printfn ppf "  mov %a, rax" pp_dest dest
    | ATuple (x1, x2, xs) ->
      printfn ppf "  mov rdi, %d ; tuple size" (2 + List.length xs);
      printfn ppf "  mov rsi, 0 ; tuple tag";
      printfn ppf "  call rukaml_alloc_block";
      List.iteri
        (fun i x ->
           (* TODO: rewrite using stack once *)
           printfn ppf "  add rsp, -8*2";
           printfn ppf "  mov qword [rsp], rax";
           helper_a (DReg "rdi") x;
           printfn ppf "  mov rax, [rsp]";
           printfn ppf "  add rsp, 8*2";
           printfn ppf "  mov qword [rax+8*%d], rdi" i)
        (x1 :: x2 :: xs);
      printfn ppf "  mov %a, rax" pp_dest dest
    | APrimitive ("match_failure", _) -> printfn ppf "  call rukaml_match_failure"
    | APrimitive ("print", (1 as arity)) ->
      emit_alloc_closure ppf (Ident.of_string "rukaml_print_int_kaml") ~arity;
      printfn ppf "  mov %a, rax" pp_dest dest
    | AArray r ->
      printfn ppf "  mov rdi, %d ; array length" (List.length r);
      printfn ppf "  mov rdi, 1 ; array tag";
      printfn ppf "  call rukaml_alloc_block";
      List.iteri
        (fun i x ->
           (* TODO: rewrite using stack once *)
           printfn ppf "  add rsp, -8*2";
           printfn ppf "  mov qword [rsp], rax";
           helper_a (DReg "rdi") x;
           printfn ppf "  mov rax, [rsp]";
           printfn ppf "  add rsp, 8*2";
           printfn ppf "  mov qword [rax+8*%d], rdi" i)
        (List.rev r);
      printfn ppf "  mov %a, rax" pp_dest dest
    | AConst (PConst_string s) ->
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
      let names = List.map (fun (ANF.APname name) -> name) pats in
      let lam_name = Ident.of_string (Printf.sprintf "__lam_%d" (gensym ())) in
      Toplevel.extend lam_name ~kind:(Function { arity = argc });
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
      emit_alloc_closure ppf lam_name ~arity:argc;
      printfn ppf "  mov %a, rax" pp_dest dest
    | atom ->
      printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__;
      failwiths "unsupported: %a" ANF.pp_a atom
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

let stdlib_externs =
  [ 2, "rukaml_alloc_closure"
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
  ; 1, "rukaml_block_size_imm"
  ; 1, "rukaml_block_tag_imm"
  ; 2, "rukaml_block_nth_imm"
  ; 0, "rukaml_match_failure"
  ; 1, "rukaml_initialize"
  ; 1, "rukaml_gc_compact"
  ; 0, "rukaml_gc_print_stats"
  ; 0, "rukaml_print_alloc_closure_count"
  ; 1, "rukaml_open_out"
  ; 1, "rukaml_close_channel"
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
  ]
;;

let stdlib_aliases =
  [ "printf", "rukaml_alloc_printf_closure"
  ; "fprintf", "rukaml_alloc_fprintf_closure"
  ; "sprintf", "rukaml_alloc_sprintf_closure"
  ; "string_len", "rukaml_string_len"
  ; "string_nth", "rukaml_string_nth"
  ; "string_equal", "rukaml_string_equal"
  ]
;;

let put_init_stdlib ppf =
  (* labels of externs are printed other way then labels of toplevel function/constants (they do not require id) *)
  List.iter
    (fun (argc, name) ->
       Toplevel.extend (Ident.ident name 0) ~kind:(Extern { argc });
       printfn ppf "extern %s" name)
    stdlib_externs;
  List.iter
    (fun (alias, aliasee) ->
       Toplevel.extend (Ident.ident alias 0) ~kind:(Alias { aliasee }))
    stdlib_aliases
;;

let emit_global_constant ppf ident expr generate_body =
  printfn ppf "section .bss";
  printfn ppf "  global %a" Toplevel.pp_label_exn ident;
  printfn ppf "  %a:    resq 1" Toplevel.pp_label_exn ident;
  printfn ppf "section .text";
  printfn ppf "init_%a:" Toplevel.pp_label_exn ident;
  printfn ppf "  push rbp";
  printfn ppf "  mov rbp, rsp";
  printfn ppf "  ; begin generate body for %a" Ident.pp ident;
  generate_body ppf expr;
  printfn ppf "  ; end generate body for %a" Ident.pp ident;
  printfn ppf "  mov qword %a, rax" Toplevel.pp_toplevel_exn ident;
  printfn ppf "  pop rbp";
  printfn ppf "  ret"
;;

let put_init_global_constants ppf =
  printfn ppf "section .text";
  printfn ppf "rukaml_init_global_constants:";
  printfn ppf "  push rbp";
  printfn ppf "  mov rbp, rsp";
  Toplevel.iter_constants (fun ident ->
    printfn ppf "  call init_%a" Toplevel.pp_label_exn ident);
  printfn ppf "  pop rbp";
  printfn ppf "  ret"
;;

let init_toplevel_table anf =
  List.iter
    (fun (_, (ident : Ident.t), body) ->
       let pats, _ = Compile_lib.ANF.group_abstractions body in
       match List.length pats with
       | _ when ident.hum_name = "main" -> Toplevel.extend ident ~kind:Main
       | 0 -> Toplevel.extend ident ~kind:Constant
       | arity -> Toplevel.extend ident ~kind:(Function { arity }))
    anf
;;

(* TODO: may be it is useless and rax should be used for DDiscard *)
let put_discard ppf =
  printfn ppf "section .bss";
  printfn ppf "  rukaml_discard: resq 1"
;;

let codegen ?(wrap_main_into_start = true) anf file =
  (* log "Going to generate code here %s %d" __FUNCTION__ __LINE__; *)
  log "ANF: @[%a@]" Compile_lib.ANF.pp_stru anf;
  init_toplevel_table anf;
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
    printfn ppf "";
    if use_custom_main
    then (
      put_print_newline ppf;
      put_print_hex ppf);
    printfn ppf "section .text";
    put_init_stdlib ppf;
    printfn ppf "";
    put_discard ppf;
    put_init_global_constants ppf;
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
      then (
        printfn ppf "section .text";
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
                    syscall|})
      else if Toplevel.is_toplevel_function name || Toplevel.is_main name
      then (
        printfn ppf "section .text";
        printfn ppf "GLOBAL %a" Toplevel.pp_label_exn name;
        printfn ppf "@[<h>%a:@]" Toplevel.pp_label_exn name;
        let pats, body = ANF.group_abstractions expr in
        let argc = List.length pats in
        let names = List.map (fun (ANF.APname name) -> name) pats in
        List.rev pats
        |> ListLabels.iteri ~f:(fun i -> function
          | ANF.APname name -> Addr_of_local.add_arg ~argc i name);
        printfn ppf "  push rbp";
        printfn ppf "  mov  rbp, rsp";
        if Toplevel.is_main name
        then (
          printfn ppf "  mov rdi, rsp";
          printfn ppf "  call rukaml_initialize";
          printfn ppf "  call rukaml_init_global_constants");
        generate_body ppf body;
        Addr_of_local.remove_args names;
        print_epilogue ppf (Format.asprintf "%a" Toplevel.pp_label_exn name))
      else if Toplevel.is_toplevel_constant name
      then emit_global_constant ppf name expr generate_body
      else assert false;
      ());
    Format.pp_print_flush ppf ());
  Result.Ok ()
;;
