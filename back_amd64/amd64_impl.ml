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
  if cfg.verbose then Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt
  else Format.ifprintf Format.std_formatter fmt

let set_verbose b = cfg.verbose <- b
let fprintf = Format.fprintf
let printfn ppf fmt = Format.kfprintf (fun ppf -> fprintf ppf "\n") ppf fmt

let print_prologue ppf name =
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

let print_epilogue ppf name =
  (* printfn ppf "  mov rsp, rbp"; *)
  printfn ppf "  pop rbp";
  printfn ppf "  ret  ;;;; %s" name;
  fprintf ppf "%!"

module Loc_of_ident : sig
  type t

  val size : unit -> int
  val lookup_exn : string -> int
  val has_key : string -> bool
  val alloc_more : unit -> int
  val put : string -> int -> unit
  val remove : string -> unit
  val alloc_and_store : string -> unit
  val alloc_temp : unit -> string
  val keys : unit -> string
  val pp : Format.formatter -> unit -> unit
end = struct
  type t = (string, int) Hashtbl.t

  (* TODO: maybe add a tracing function, which prints the stack with names *)
  let store : t = Hashtbl.create 23
  let size () = Hashtbl.length store
  let allocated = ref 0
  let has_key name = Hashtbl.mem store name

  let lookup_exn name =
    try
      let saved = Hashtbl.find store name in
      assert (!allocated >= saved);
      !allocated - saved
    with Not_found ->
      failwiths "Loc_of_ident.lokkup_exn: name %S is absent" name

  let alloc_more () =
    incr allocated;
    !allocated

  let gensym =
    let last = ref 0 in
    fun () ->
      incr last;
      Printf.sprintf "__temp%d" !last

  let put name shift = Hashtbl.add store name shift

  let remove name =
    assert (Hashtbl.mem store name);
    Hashtbl.remove store name;
    decr allocated

  let alloc_and_store name = put name (alloc_more ())

  let alloc_temp () =
    let name = gensym () in
    alloc_and_store name;
    name

  let keys () =
    Hashtbl.to_seq_keys store |> Seq.fold_left (fun acc x -> acc ^ " " ^ x) ""

  let pp ppf () =
    let b = Buffer.create 100 in

    Printf.bprintf b "@[{stack|";
    Hashtbl.iter (Printf.bprintf b "%s -> %d; ") store;
    Printf.bprintf b "|stack}@]";
    Format.fprintf ppf "%s" (Buffer.contents b)
end

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

type dest = DReg of string | DStack_var of int

module Addr_of_local = struct
  let store = Hashtbl.create 13
  let last_pos = ref 0
  let get_locals_count () = !last_pos

  let clear () =
    log "%s" __FUNCTION__;
    Hashtbl.clear store;
    last_pos := 0

  let extend name =
    incr last_pos;
    log "extend %s with shift = %d" name !last_pos;
    Hashtbl.add store name !last_pos

  let remove_local name =
    let pos = Hashtbl.find store name in
    if !last_pos = pos then (
      log "remove %s with shift = %d" name !last_pos;
      decr last_pos;
      Hashtbl.remove store name)
    else
      failwiths "Something bad %d. Can't remove local variable %S" __LINE__ name

  let count () = Hashtbl.length store
  let contains name = Hashtbl.mem store name

  let find_exn name =
    match Hashtbl.find store name with
    | v -> v
    | exception Not_found ->
        failwiths "Can't find location of a variable %S" name

  let add_arg ~argc i name =
    assert (i < argc);
    let loc = -2 - argc + 1 + i in
    assert (loc < 0);
    log "Location argument %S in [rbp+%d]" name (-loc);
    Hashtbl.add store name loc

  let remove_args xs =
    log "Removing info about args [ %a ]"
      (Format.pp_print_list Format.pp_print_string)
      xs;
    List.iter (Hashtbl.remove store) xs

  let pp_local_exn ppf name =
    let offset = find_exn name in
    if offset > 0 then fprintf ppf "[rbp-%d*8]" offset
    else fprintf ppf "[rbp+%d*8]" (-offset)
end

let pp_dest ppf = function
  | DReg s -> fprintf ppf "%s" s
  | DStack_var offset ->
      (* 8 for 64 bit, 4 for 32bit *)
      if offset = 0 then fprintf ppf "[rbp]"
      else if offset > 0 then fprintf ppf "[rbp-%d*8]" offset
      else fprintf ppf "[rbp+%d*8]" (-offset)

(* let dealloc_var ppf name =
   Loc_of_ident.remove name;
   printfn ppf "  add rsp, 8 ; deallocate var %S" name *)

let alloc_closure ppf name arity =
  printfn ppf "  mov rdi, %s" name;
  printfn ppf "  mov rsi, %d" arity;
  printfn ppf "  call rukaml_alloc_closure"

let allocate_locals ppf input_anf : now:unit -> unit =
  let open Compile_lib.ANF2 in
  let names = Hashtbl.create 23 in
  let rec helper = function
    | EComplex c -> helper_c c
    | ELet (_flg, PVar name, rhs, where_) ->
        Addr_of_local.extend name;
        Hashtbl.add names name ();
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
  let count = Hashtbl.length names in

  assert (count = Addr_of_local.get_locals_count ());

  (* If assertion fails it's like a number of locals with the same names *)
  let args_repr =
    Hashtbl.to_seq_keys names |> List.of_seq |> String.concat ", "
  in
  let deallocate_padding =
    if count mod 2 = 1 then (
      let pad_name = Printf.sprintf "__pad%d" (gensym ()) in
      printfn ppf "  sub rsp, 8 ; allocate padding for locals";
      Addr_of_local.extend pad_name;
      fun () ->
        Addr_of_local.remove_local pad_name;
        printfn ppf "  add rsp, 8 ; deallocate padding for locals")
    else fun () -> ()
  in

  if count > 0 then (
    let () =
      printfn ppf "  sub rsp, 8*%d ; allocate for local variables %s" count
        args_repr
    in
    fun ~now ->
      let () = now in
      deallocate_padding ();
      printfn ppf "  add rsp, 8*%d ; deallocate local variables %s" count
        args_repr)
  else fun ~now ->
    let () = now in
    ()

(**
    Argument [is_toplevel] returns None or Some arity. *)
let generate_body is_toplevel ppf body =
  let open Compile_lib.ANF2 in
  let open Miniml.Parsetree in
  let allocate_args args =
    let count = List.length args in
    let _stack_padding =
      if count mod 2 = 0 then 0
      else (
        printfn ppf "  sub rsp, 8 ; trying to save alignment 16 bytes";
        1)
    in

    printfn ppf "  sub rsp, 8*%d ; fun arguments" count;
    List.iteri
      (fun i ->
        let pp_access v =
          printfn ppf "  mov qword [rsp+8*%d], %d" (count - 1 - i) v
        in
        function
        | AUnit | AConst (PConst_bool false) -> pp_access 0
        | AConst (PConst_bool true) -> pp_access 1
        | AConst (PConst_int n) -> pp_access n
        | ALam _ -> failwith "Should it be representable in ANF?"
        | AVar _ -> assert false
        | APrimitive _ -> assert false
        | ATuple _ -> assert false)
      args;
    count + _stack_padding
  in

  let rec helper dest = function
    | EComplex c -> helper_c dest c
    | ELet (_, Miniml.Parsetree.PVar name, rhs, wher) ->
        assert (Addr_of_local.contains name);
        Loc_of_ident.alloc_and_store name;
        helper_c (DStack_var (Addr_of_local.find_exn name)) rhs;
        helper dest wher (* dealloc_var ppf name *)
    | ELet (_, PTuple (_, _, _), _, _) -> assert false
  and helper_c (dest : dest) = function
    | CIte (AConst (Miniml.Parsetree.PConst_bool true), bth, _bel) ->
        helper dest bth
    | CIte (AConst (Miniml.Parsetree.PConst_bool false), _bth, bel) ->
        helper dest bel
    | CIte (AVar econd, bth, bel) when Loc_of_ident.has_key econd ->
        (* if on global or local variable  *)
        printfn ppf "  mov rdx, [rsp+%d*8] " (Loc_of_ident.lookup_exn econd);
        printfn ppf "  cmp rdx, 0";
        let el_lab = Printf.sprintf "lab_then_%d" (gensym ()) in
        let fin_lab = Printf.sprintf "lab_endif_%d" (gensym ()) in
        printfn ppf "  je %s" el_lab;
        helper dest bth;
        printfn ppf "  jmp %s" fin_lab;
        printfn ppf "  %s:" el_lab;
        helper dest bel;
        printfn ppf "  %s:" fin_lab
    | CApp (AVar ("print" as f), arg1, [])
      when is_toplevel f = None && not (Loc_of_ident.has_key f) -> (
        match arg1 with
        | AVar v when Loc_of_ident.has_key v ->
            printfn ppf "  mov rdi, %a" pp_dest
              (DStack_var (Addr_of_local.find_exn v));
            printfn ppf "  call rukaml_print_int ; short";
            printfn ppf "  mov %a, rax" pp_dest dest
        | AConst (PConst_int n) ->
            printfn ppf "  mov rdi, %d" n;
            printfn ppf "  call rukaml_print_int";
            printfn ppf "  mov %a, rax" pp_dest dest
        | AConst (PConst_bool _)
        | AVar _ | APrimitive _
        | ATuple (_, _, _)
        | ALam (_, _)
        | AUnit ->
            failwith "Should not happen")
    | CApp (AVar f, arg1, args) when Option.is_some (is_toplevel f) ->
        (* Callig a rukaml function uses custon calling convention.
           Pascal convention: all arguments on stack, LTR *)
        let expected_arity = Option.get (is_toplevel f) in
        let formal_arity = 1 + List.length args in

        (* printfn ppf "\t; expected_arity = %d\n\t; formal_arity = %d"
             expected_arity formal_arity;
           printfn ppf "\t; calling %S" f; *)
        if expected_arity = formal_arity then (
          let to_remove = allocate_args (arg1 :: args) in

          printfn ppf "  call %s" f;
          printfn ppf "  add rsp, 8*%d ; dealloc args" to_remove;
          printfn ppf "  mov %a, rax" pp_dest dest)
        else if formal_arity < expected_arity then (
          printfn ppf "  mov rdi, %s" f;
          printfn ppf "  mov rsi, %d" expected_arity;
          printfn ppf "  call rukaml_alloc_closure";

          (* Addr_of_local.extend "closure_container"; *)
          (* Addr_of_local.extend "temp_for_padding"; *)
          (* printfn ppf "  mov %a, rax" pp_dest
             (DStack_var (Addr_of_local.find_exn "closure_container")); *)
          let partial_args_count = allocate_args (arg1 :: args) in

          printfn ppf "  mov rdi, rax";
          printfn ppf "  mov rsi, %d" formal_arity;
          assert (formal_arity < 5);
          (* See calling convention *)
          List.iteri
            (fun i rname ->
              printfn ppf "  mov %s, [rsp+8*%d]" rname (formal_arity - i - 1))
            (list_take formal_arity [ "rdx"; "rcx"; "r8"; "r9" ]);
          printfn ppf "  mov al, 0";
          printfn ppf "  call rukaml_applyN";
          printfn ppf "  add rsp, 8*%d ; deallocate args of rukaml_applyN"
            partial_args_count;
          printfn ppf "  mov %a, rax" pp_dest dest
          (* printfn ppf "  sub rsp, 8*2 ; deallocate closure value and padding" *))
        else failwith "Arity mismatch: over application"
    | CApp (APrimitive "=", arg1, [ arg2 ]) ->
        failwiths "not implemented %d" __LINE__
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
    | CApp (APrimitive "-", arg1, [ AConst (PConst_int 1) ]) ->
        failwiths "not implemented %d" __LINE__
        (* let left_name = LoI.alloc_temp () in
           printfn ppf "  sub rsp, 8 ; allocate for var %S" left_name;
           let left_dest = DStack_var left_name in
           helper_a left_dest arg1;
           printfn ppf "  mov rax, %a" pp_dest left_dest;
           printfn ppf "  dec rax";
           printfn ppf "  mov %a, rax" pp_dest dest;
           dealloc_var ppf left_name *)
    | CApp (APrimitive "-", arg1, [ AConst (PConst_int n) ]) ->
        failwiths "not implemented %d" __LINE__
    (* TODO: Move this specialization to the case below  *)
    (* let left_name = LoI.alloc_temp () in
       printfn ppf "  sub rsp, 8 ; allocate for var %S" left_name;
       let left_dest = DStack_var left_name in
       helper_a left_dest arg1;
       printfn ppf "  mov rax, %a" pp_dest left_dest;
       printfn ppf "  mov qword r8, %d" n;
       printfn ppf "  sub rax, r8";
       printfn ppf "  mov %a, rax" pp_dest dest;
       dealloc_var ppf left_name *)
    | CApp
        (APrimitive (("+" | "*") as prim), AVar vname, [ AConst (PConst_int n) ])
    | CApp
        (APrimitive (("+" | "*") as prim), AConst (PConst_int n), [ AVar vname ])
      -> (
        match is_toplevel vname with
        | None ->
            printfn ppf "  mov qword r11, %a" Addr_of_local.pp_local_exn vname;
            printfn ppf "  %s r11, %d"
              (match prim with
              | "+" -> "add "
              | "*" -> "imul"
              | op ->
                  Format.asprintf ";;; TODO %s. %s %d" op __FUNCTION__ __LINE__)
              n;
            printfn ppf "  mov qword %a, r11" pp_dest dest
        | Some _ ->
            failwiths "not implemented %d" __LINE__
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
               (match prim with
               | "+" -> printfn ppf "  add  r8, rax"
               | "*" -> printfn ppf "  imul r8, rax"
               | op -> printfn ppf ";;; TODO %s. %s %d" op __FUNCTION__ __LINE__);
               printfn ppf "  mov %a, r8" pp_dest dest;
               dealloc_var ppf right_name;
               dealloc_var ppf left_name *))
    | CApp (AVar f, (AConst _ as arg), []) | CApp (AVar f, (AVar _ as arg), [])
      ->
        assert (Option.is_none (is_toplevel f));

        Addr_of_local.extend "temp_padding";
        Addr_of_local.extend "arg1";
        printfn ppf "  sub rsp, 8 ; padding";
        printfn ppf "  sub rsp, 8 ; first arg of a function %s" f;
        helper_a (DStack_var (Addr_of_local.find_exn "arg1")) arg;

        printfn ppf "  mov rax, 0  ; no float arguments";
        printfn ppf "  mov rdi, %a" pp_dest
          (DStack_var (Addr_of_local.find_exn f));
        printfn ppf "  mov rsi, 1";
        printfn ppf "  mov rdx, %a" pp_dest
          (DStack_var (Addr_of_local.find_exn "arg1"));
        printfn ppf "  call rukaml_applyN";
        Addr_of_local.remove_local "arg1";
        Addr_of_local.remove_local "temp_padding";
        printfn ppf "  add rsp, 8*2 ; free space for args of function %S" f;
        printfn ppf "  mov %a, rax" pp_dest dest
    | CApp (APrimitive "field", AConst (PConst_int n), [ (AVar _ as cont) ]) ->
        helper_a (DReg "rsi") cont;
        printfn ppf "  mov rdi, %d" n;
        printfn ppf "  call rukaml_field";
        printfn ppf "  mov %a, rax" pp_dest dest
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
        printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__;
        printfn ppf ";;; %a" pp_c anf
    | rest ->
        printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__;
        printfn ppf "; @[<h>%a@]" pp_c rest
  and helper_a (dest : dest) = function
    | AConst (Miniml.Parsetree.PConst_bool true) ->
        printfn ppf "  mov qword %a, 1" pp_dest dest
    | AConst (Miniml.Parsetree.PConst_int n) ->
        printfn ppf "  mov qword %a,  %d" pp_dest dest n
    | AVar vname -> (
        match is_toplevel vname with
        | None ->
            (* We use temprarily rdx because we can't move from stack to stack *)
            printfn ppf "  mov rdx, [rsp+%d*8] " (Addr_of_local.find_exn vname);
            printfn ppf "  mov %a, rdx ; access a var %S" pp_dest dest vname
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
        printfn ppf ";;; @[`%a`@]" pp_a atom
  in
  let dealloc_locals = allocate_locals ppf body in
  helper (DReg "rax") body;
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
      printfn ppf "section .note.GNU-stack noalloc noexec nowrite progbits";

      if use_custom_main then
        printfn ppf
          {|section .data
            newline_char: db 10
            codes: db '0123456789abcdef' |};
      printfn ppf "section .text";
      (* printfn ppf "global _start"; *)
      if use_custom_main then (
        put_print_newline ppf;
        put_print_hex ppf);

      (* externs *)
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
      printfn ppf "";

      if use_custom_main then
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
      else if wrap_main_into_start then
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
             if Loc_of_ident.size () <> 0 then
               failwiths
                 "There are left over variables (before function %s): %s " name
                 (Loc_of_ident.keys ());

             (* printfn ppf "";
                fprintf ppf "\t; %a\n" Loc_of_ident.pp (); *)

             (* print_prologue ppf name; *)

             (* fprintf ppf "  ; There are %d known arguments in %s\n%!"
                (Loc_of_ident.size ()) name; *)

             (* printfn ppf "  sub rsp, %d" (8 * Loc_of_ident.size ()); *)
             (if use_custom_main && name = "main" then
                printfn ppf
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
              else
                let () = printfn ppf "GLOBAL %s" name in
                let () = printfn ppf "@[<h>%s:@]" name in

                let pats, body = ANF2.group_abstractions expr in

                (* TODO: Put arguments into Addr_of_local using RTL order *)
                let argc = List.length pats in
                let names =
                  List.map (function ANF2.APname name -> name) pats
                in
                List.rev pats
                |> ListLabels.iteri ~f:(fun i -> function
                     | ANF2.APname name -> Addr_of_local.add_arg ~argc i name);

                (* let rsi_goes_here = Loc_of_ident.alloc_temp () in *)
                printfn ppf "  push rbp";
                printfn ppf "  mov  rbp, rsp";
                if name = "main" then (
                  printfn ppf "  mov rdi, rsp";
                  printfn ppf "  call rukaml_initialize");
                (* let rbp_goes_here = Loc_of_ident.alloc_temp () in *)
                generate_body is_toplevel ppf body;
                Addr_of_local.remove_args names;

                (* let () =
                     (* deallocation from stack should be done by caller  *)
                     List.iter
                       (function
                         | ANF2.APname name ->
                             Loc_of_ident.remove name (* dealloc_var ppf name *))
                       (List.rev pats)
                   in *)
                print_epilogue ppf name);
             ());
      Format.pp_print_flush ppf ());

  Result.Ok ()
