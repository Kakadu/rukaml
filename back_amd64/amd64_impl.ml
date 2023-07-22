(* https://flint.cs.yale.edu/cs421/papers/x86-asm/asm.html
   https://jvns.ca/blog/2021/05/17/how-to-look-at-the-stack-in-gdb
   https://en.wikipedia.org/wiki/X86_calling_conventions
   https://github.com/jhucompilers/fall2022/tree/gh-pages/lectures
*)

(* We are using Intel syntax !! *)

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
  val alloc_more : unit -> int
  val put : string -> int -> unit
  val remove : string -> unit
  val alloc_and_store : string -> unit
  val alloc_temp : unit -> string
  val keys : unit -> string
end = struct
  type t = (string, int) Hashtbl.t

  let store : t = Hashtbl.create 23
  let size () = Hashtbl.length store
  let allocated = ref 0

  let lookup_exn name =
    let saved = Hashtbl.find store name in
    assert (!allocated >= saved);
    !allocated - saved

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
    Hashtbl.remove store name

  let alloc_and_store name = put name (alloc_more ())

  let alloc_temp () =
    let name = gensym () in
    alloc_and_store name;
    name

  let keys () =
    Hashtbl.to_seq_keys store |> Seq.fold_left (fun acc x -> acc ^ " " ^ x) ""
end

type dest = DReg of string | DStack_var of string

let pp_dest ppf = function
  | DReg s -> fprintf ppf "%s" s
  | DStack_var v ->
      (* 8 for 64 bit, 4 for 32bit *)
      fprintf ppf "[%d+rsp]" (8 * Loc_of_ident.lookup_exn v)

let dealloc_var ppf name =
  Loc_of_ident.remove name;
  printfn ppf "  add rsp, 8 ; deallocate var %S" name

let generate_body ppf body =
  let open Compile_lib.ANF2 in
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
        let aname = LoI.alloc_temp () in
        printfn ppf "  sub rsp, 8 ; allocate for var %S" aname;
        helper_a (DStack_var aname) arg;
        (* TODO: distinguish global functions and arguments *)
        printfn ppf "  call %s" f;
        dealloc_var ppf aname
        (* Need to worry about caller-save registers here  *)
    | CApp _ -> printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__
    | _ -> printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__
  and helper_a (dest : dest) = function
    | AConst (Miniml.Parsetree.PConst_int n) ->
        printfn ppf "  mov qword %a,  %d" pp_dest dest n
    | AVar vname ->
        (* We use temprarily rdx because we can't move from stack to stack *)
        printfn ppf "  mov rdx, [rsp+%d*8] " (LoI.lookup_exn vname);
        printfn ppf "  mov %a, rdx ; access a var %S" pp_dest dest vname
    | _ -> printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__
  in

  helper (DReg "rax") body

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

let codegen anf file =
  (* log "Going to generate code here %s %d" __FUNCTION__ __LINE__; *)
  log "ANF: @[%a@]" Compile_lib.ANF2.pp_stru anf;

  Stdio.Out_channel.with_file file ~f:(fun ch ->
      let ppf = Format.formatter_of_out_channel ch in

      printfn ppf
        {|section .data
            newline_char: db 10
            codes: db '0123456789abcdef' |};
      printfn ppf "section .text";
      printfn ppf "global _start";
      printfn ppf
        {|print_newline:
          mov rax, 1 ; 'write' syscall identifier
          mov rdi, 1 ; stdout file descriptor
          mov rsi, newline_char ; where do we take data from
          mov rdx, 1 ; the amount of bytes to write
          syscall
          ret |};
      put_print_hex ppf;

      if use_custom_main then
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
      else
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
                let () = printfn ppf "%s:" name in

                let pats, body = ANF2.group_abstractions expr in

                List.iter
                  (function
                    | ANF2.APname name ->
                        let n = Loc_of_ident.alloc_more () in
                        Loc_of_ident.put name n
                        (* printfn ppf "\t; Variable %S allocated on stack" name *))
                  pats;
                printfn ppf "  push rbp";
                printfn ppf "  mov  rbp, rsp";
                let rbp_goes_here = Loc_of_ident.alloc_temp () in
                let rsi_goes_here = Loc_of_ident.alloc_temp () in
                generate_body ppf body;
                Loc_of_ident.remove rsi_goes_here;
                Loc_of_ident.remove rbp_goes_here;

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
