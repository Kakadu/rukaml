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

type dest = DReg of string | DStack_var of string

let pp_dest ppf = function
  | DReg s -> fprintf ppf "%s" s
  | DStack_var v ->
      let shift = Loc_of_ident.lookup_exn v in
      (* 8 for 64 bit, 4 for 32bit *)
      if shift = 0 then fprintf ppf "[rsp]" else fprintf ppf "[8*%d+rsp]" shift

let dealloc_var ppf name =
  Loc_of_ident.remove name;
  printfn ppf "  add rsp, 8 ; deallocate var %S" name

let alloc_closure ppf name arity =
  printfn ppf "  mov rdi, %s" name;
  printfn ppf "  mov rsi, %d" arity;
  printfn ppf "  call rukaml_alloc_closure"

(**
    Argument [is_toplevel] returns None or Some arity. *)
let generate_body is_toplevel ppf body =
  let open Compile_lib.ANF2 in
  let open Miniml.Parsetree in
  let module LoI = Loc_of_ident in
  let rec helper dest = function
    | EComplex c ->
        (* printfn ppf ";;; TODO Complex " *)
        helper_c dest c
    | ELet (_, Miniml.Parsetree.PVar name, rhs, wher) ->
        Loc_of_ident.alloc_and_store name;
        printfn ppf "  sub rsp, 8 ; allocate for var %S" name;
        helper_c (DStack_var name) rhs;
        helper dest wher;
        dealloc_var ppf name
    | ELet (_, PTuple (_, _, _), _, _) -> assert false
  and helper_c (dest : dest) = function
    | CIte (AConst (Miniml.Parsetree.PConst_bool true), bth, _bel) ->
        helper dest bth
    | CIte (AConst (Miniml.Parsetree.PConst_bool false), _bth, bel) ->
        helper dest bel
    | CIte (AVar econd, bth, bel) when LoI.has_key econd ->
        printfn ppf "  mov rdx, [rsp+%d*8] " (LoI.lookup_exn econd);
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
      when is_toplevel f = None && not (LoI.has_key f) -> (
        match arg1 with
        | AVar v when LoI.has_key v ->
            printfn ppf "  mov rdi, %a" pp_dest (DStack_var v);
            printfn ppf "  call rukaml_print_int ; short";
            printfn ppf "  mov %a, rax" pp_dest dest
        | arg1 ->
            let arg_name = LoI.alloc_temp () in
            printfn ppf "  sub rsp, 8 ; allocate for var %S" arg_name;
            let arg_dest = DStack_var arg_name in
            helper_a arg_dest arg1;
            printfn ppf "  mov rdi, %a" pp_dest arg_dest;
            printfn ppf "  call rukaml_print_int";
            printfn ppf "  mov %a, rax" pp_dest dest;
            dealloc_var ppf arg_name)
    | CApp (AVar f, arg1, args) when Option.is_some (is_toplevel f) ->
        let expected_arity = Option.get (is_toplevel f) in
        let formal_arity = 1 + List.length args in

        printfn ppf "\t; expected_arity = %d\n\t; formal_arity = %d"
          expected_arity formal_arity;
        printfn ppf "\t; calling %S" f;
        if expected_arity = formal_arity then (
          printfn ppf "  ; expected_arity = formal_arity = %d" formal_arity;
          let formal_locs =
            List.init formal_arity (fun i ->
                let name = LoI.alloc_temp () in
                printfn ppf
                  "  sub rsp, 8 ; allocate for argument %d (name = %s)" i name;
                name)
          in
          let () =
            List.rev (arg1 :: args)
            |> List.iter2
                 (fun name arg -> helper_a (DStack_var name) arg)
                 formal_locs
          in
          printfn ppf "  call %s" f;
          List.iter (dealloc_var ppf) formal_locs;
          printfn ppf "  mov %a, rax" pp_dest dest)
        else if formal_arity < expected_arity then (
          let wfname = LoI.alloc_temp () in
          printfn ppf "  sub rsp, 8 ; allocate wrapper for func %s" wfname;
          printfn ppf "  mov rdi, %s" f;
          printfn ppf "  mov rsi, %d" expected_arity;
          printfn ppf "  call rukaml_alloc_closure";
          printfn ppf "  mov %a, rax" pp_dest (DStack_var wfname);
          let formal_locs =
            List.mapi
              (fun i _arg ->
                let name = LoI.alloc_temp () in
                printfn ppf
                  "  sub rsp, 8 ; allocate for argument %d (name = %s)" i name;
                name)
              (arg1 :: args)
          in
          List.iter2
            (fun loc arg -> helper_a (DStack_var loc) arg)
            formal_locs (arg1 :: args);
          printfn ppf "  mov rdi, %a" pp_dest (DStack_var wfname);
          printfn ppf "  mov rsi, %d" formal_arity;
          assert (formal_arity < 5);
          (* See calling convention *)
          List.iter2
            (fun loc rname ->
              printfn ppf "  mov %s, %a" rname pp_dest (DStack_var loc))
            formal_locs
            (list_take (List.length formal_locs) [ "rdx"; "rcx"; "r8"; "r9" ]);
          printfn ppf "  mov al, 0";
          printfn ppf "  call rukaml_applyN";
          printfn ppf "  mov %a, rax" pp_dest dest;

          let () = List.iter (fun name -> dealloc_var ppf name) formal_locs in
          dealloc_var ppf wfname)
        else failwith "Arity mismatch: over application"
    | CApp (APrimitive "=", arg1, [ arg2 ]) ->
        let left_name = LoI.alloc_temp () in
        printfn ppf "  sub rsp, 8 ; allocate for var %S" left_name;
        let left_dest = DStack_var left_name in
        helper_a left_dest arg1;
        let right_name = LoI.alloc_temp () in
        printfn ppf "  sub rsp, 8 ; allocate for var %S" right_name;
        let right_dest = DStack_var right_name in
        helper_a right_dest arg2;
        printfn ppf "  mov rax, %a" pp_dest left_dest;
        printfn ppf "  mov rbx, %a" pp_dest right_dest;
        printfn ppf "  cmp rax, rbx";
        let eq_lab = Printf.sprintf "lab_%d" (gensym ()) in
        let exit_lab = Printf.sprintf "lab_%d" (gensym ()) in
        printfn ppf "  je %s" eq_lab;
        printfn ppf "  mov qword %a, 0" pp_dest dest;
        printfn ppf "  jmp %s" exit_lab;
        printfn ppf "  %s:" eq_lab;
        printfn ppf "    mov qword %a, 1" pp_dest dest;
        printfn ppf "    jmp %s" exit_lab;
        printfn ppf "  %s:" exit_lab;
        dealloc_var ppf right_name;
        dealloc_var ppf left_name
    | CApp (APrimitive "-", arg1, [ AConst (PConst_int 1) ]) ->
        let left_name = LoI.alloc_temp () in
        printfn ppf "  sub rsp, 8 ; allocate for var %S" left_name;
        let left_dest = DStack_var left_name in
        helper_a left_dest arg1;
        printfn ppf "  mov rax, %a" pp_dest left_dest;
        printfn ppf "  dec rax";
        printfn ppf "  mov %a, rax" pp_dest dest;
        dealloc_var ppf left_name
    | CApp (APrimitive "-", arg1, [ AConst (PConst_int n) ]) ->
        (* TODO: Move this specialization to the case below  *)
        let left_name = LoI.alloc_temp () in
        printfn ppf "  sub rsp, 8 ; allocate for var %S" left_name;
        let left_dest = DStack_var left_name in
        helper_a left_dest arg1;
        printfn ppf "  mov rax, %a" pp_dest left_dest;
        printfn ppf "  mov qword rbx, %d" n;
        printfn ppf "  sub rax, rbx";
        printfn ppf "  mov %a, rax" pp_dest dest;
        dealloc_var ppf left_name
    | CApp (APrimitive (("+" | "*") as prim), arg1, [ arg2 ]) ->
        let left_name = LoI.alloc_temp () in
        printfn ppf "  sub rsp, 8 ; allocate for var %S" left_name;
        let left_dest = DStack_var left_name in
        helper_a left_dest arg1;
        let right_name = LoI.alloc_temp () in
        printfn ppf "  sub rsp, 8 ; allocate for var %S" right_name;
        let right_dest = DStack_var right_name in
        helper_a right_dest arg2;
        printfn ppf "  mov rax, %a" pp_dest left_dest;
        printfn ppf "  mov rbx, %a" pp_dest right_dest;
        (match prim with
        | "+" -> printfn ppf "  add  rbx, rax"
        | "*" -> printfn ppf "  imul rbx, rax"
        | op -> printfn ppf ";;; TODO %s. %s %d" op __FUNCTION__ __LINE__);
        printfn ppf "  mov %a, rbx" pp_dest dest;
        dealloc_var ppf right_name;
        dealloc_var ppf left_name
    | CApp (AVar f, (AConst _ as arg), []) | CApp (AVar f, (AVar _ as arg), [])
      ->
        assert (Option.is_none (is_toplevel f));
        let aname = LoI.alloc_temp () in
        printfn ppf "  sub rsp, 8 ; allocate for var %S" aname;
        helper_a (DStack_var aname) arg;

        printfn ppf "  mov rax, 0  ; no float arguments";
        printfn ppf "  mov rdi, %a" pp_dest (DStack_var f);
        printfn ppf "  mov rsi, 1";
        printfn ppf "  mov rdx, %a" pp_dest (DStack_var aname);
        printfn ppf "  call rukaml_applyN";
        dealloc_var ppf aname;
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
        (* printfn ppf "  mov qword %a, 0" pp_dest dest *)
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
            printfn ppf "  mov rdx, [rsp+%d*8] " (LoI.lookup_exn vname);
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

  helper (DReg "rax") body

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
  pop rax          ; ˆ see line 24 ˆ
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
             printfn ppf "";
             fprintf ppf "\t; %a\n" Loc_of_ident.pp ();

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

                (* We are doing reverse to be more look like UNIX calling conversion.
                   So the 1st argument is closer to the top of the stach than the last arg
                *)
                List.rev pats
                |> List.iter (function ANF2.APname name ->
                       let n = Loc_of_ident.alloc_more () in
                       Loc_of_ident.put name n);
                let rsi_goes_here = Loc_of_ident.alloc_temp () in
                printfn ppf "  push rbp";
                printfn ppf "  mov  rbp, rsp";
                if name = "main" then (
                  printfn ppf "mov rdi, rsp";
                  printfn ppf "call rukaml_initialize");
                let rbp_goes_here = Loc_of_ident.alloc_temp () in
                generate_body is_toplevel ppf body;
                Loc_of_ident.remove rbp_goes_here;
                Loc_of_ident.remove rsi_goes_here;

                let () =
                  (* deallocation from stack should be done by caller  *)
                  List.iter
                    (function
                      | ANF2.APname name ->
                          Loc_of_ident.remove name (* dealloc_var ppf name *))
                    (List.rev pats)
                in

                print_epilogue ppf name);
             ());
      Format.pp_print_flush ppf ());

  Result.Ok ()
