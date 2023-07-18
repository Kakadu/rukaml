(* https://flint.cs.yale.edu/cs421/papers/x86-asm/asm.html *)

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
  printfn ppf "  mov rbp, rsp";
  (* printfn ppf "  ;sub rsp, 24 ; given 24 is total size of local variables"; *)
  fprintf ppf "%!"

let print_epilogue ppf name =
  printfn ppf "  mov rsp, rbp";
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
end

type dest = DReg of string | DStack_var of string

let pp_dest ppf = function
  | DReg s -> fprintf ppf "%%%s" s
  | DStack_var v ->
      (* 8 for 64 bit, 4 for 32bit *)
      fprintf ppf "%d(%%rsi)" (8 * Loc_of_ident.lookup_exn v)

let dealloc_var ppf name =
  Loc_of_ident.remove name;
  printfn ppf "  addq $8, %%rsp ; deallocate var %S" name

let generate_body ppf body =
  let open Compile_lib.ANF2 in
  let module LoI = Loc_of_ident in
  let rec helper dest = function
    | EComplex c ->
        (* printfn ppf ";;; TODO Complex " *)
        helper_c dest c
    | ELet (_, Miniml.Parsetree.PVar name, rhs, wher) ->
        Loc_of_ident.alloc_and_store name;
        printfn ppf "  subq $8, %%rsp ; allocate for var %S" name;
        helper_c (DStack_var name) rhs;
        helper dest wher;
        dealloc_var ppf name
    | ELet (_, PTuple (_, _, _), _, _) -> assert false
  and helper_c (dest : dest) = function
    | CApp (APrimitive "*", arg1, [ arg2 ]) ->
        let left_name = LoI.alloc_temp () in
        let left_dest = DStack_var left_name in
        helper_a left_dest arg1;
        let right_name = LoI.alloc_temp () in
        let right_dest = DStack_var right_name in
        helper_a right_dest arg2;
        printfn ppf "  mov %a, %%rax" pp_dest left_dest;
        printfn ppf "  mov %a, %%rbx" pp_dest right_dest;
        printfn ppf "  add %%rax, %%rbx";
        printfn ppf "  mov %%rbx, %a" pp_dest dest;
        dealloc_var ppf right_name;
        dealloc_var ppf left_name
    | CApp (AVar f, AVar arg, []) ->
        (* Need to worry about caller-save registers here  *)
        printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__
    | CApp _ -> printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__
    (* printfn ppf "; @[%a@]" Compile_lib.ANF2.pp_c expr *)
    | _ -> printfn ppf ";;; TODO %s %d" __FUNCTION__ __LINE__
  and helper_a (dest : dest) = function AConst _ -> () | _ -> () in

  helper (DReg "rax") body

let codegen anf file =
  log "Going to generate code here %s %d" __FUNCTION__ __LINE__;
  log "ANF: @[%a@]" Compile_lib.ANF2.pp_stru anf;

  Stdio.Out_channel.with_file file ~f:(fun ch ->
      let ppf = Format.formatter_of_out_channel ch in

      printfn ppf "section .text";
      let open Compile_lib in
      anf
      |> List.iter (fun (_flg, name, expr) ->
             if Loc_of_ident.size () <> 0 then
               failwiths "There are left over variables (before function )... ";
             printfn ppf "";
             print_prologue ppf name;
             let pats, body = ANF2.group_abstractions expr in

             List.iter
               (function
                 | ANF2.APname name ->
                     let n = Loc_of_ident.alloc_more () in
                     Loc_of_ident.put name n;
                     printfn ppf "\tVariable %S allocated on stack" name)
               pats;
             fprintf ppf "  ; There are %d known arguments in %s\n%!"
               (Loc_of_ident.size ()) name;

             (* printfn ppf "  sub rsp, %d" (8 * Loc_of_ident.size ()); *)
             generate_body ppf body;

             List.iter
               (function ANF2.APname name -> dealloc_var ppf name)
               (List.rev pats);

             print_epilogue ppf name;
             ());
      Format.pp_print_flush ppf ());

  Result.Ok ()
