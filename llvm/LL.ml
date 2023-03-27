open Llvm
open Printf

type binop = ?name:string -> llvalue -> llvalue -> llvalue

module type S = sig
  val build_store : llvalue -> llvalue -> llvalue
  val build_call : ?name:string -> llvalue -> llvalue list -> llvalue
  val lookup_func_exn : string -> llvalue
  val build_add : binop
  val build_sub : binop
  val build_mul : binop
  val build_sdiv : binop [@@inline]
  val build_eq : binop
  val declare_function : string -> lltype -> llvalue
  val position_at_end : llbasicblock -> unit
  val build_ret : llvalue -> llvalue
  (* val build_function: ?name:string -> llvalue  *)

  val build_load : ?name:string -> llvalue -> llvalue
  val build_ptrtoint : ?name:string -> llvalue -> lltype -> llvalue
  val build_inttoptr : ?name:string -> llvalue -> lltype -> llvalue
  val build_pointercast : ?name:string -> llvalue -> lltype -> llvalue
  val const_int : lltype -> int -> llvalue
  val const_bool : lltype -> bool -> llvalue
  val params : llvalue -> llvalue array
end

let make builder module_ =
  let module L : S = struct
    let build_store a b = Llvm.build_store a b builder
    let build_call ?(name = "") f args = build_call f (Array.of_list args) name builder

    let lookup_func_exn fname =
      match lookup_function fname module_ with
      | Some f -> f
      | None -> failwith (sprintf "Function '%s' not found" fname)
    ;;

    let build_binop op_builder ?(name = "") l r = op_builder l r name builder
    let build_add = build_binop build_add
    let build_sub = build_binop build_sub
    let build_mul = build_binop build_mul
    let build_sdiv = build_binop build_sdiv
    let build_eq = build_binop (build_icmp Icmp.Eq)
    let build_ptrtoint ?(name = "") e typ = Llvm.build_ptrtoint e typ name builder
    let build_inttoptr ?(name = "") e typ = Llvm.build_inttoptr e typ name builder
    let build_pointercast ?(name = "") f typ = Llvm.build_pointercast f typ name builder
    let declare_function name ftype = declare_function name ftype module_
    let position_at_end basic_block = position_at_end basic_block builder
    let build_ret ret_val = build_ret ret_val builder
    let build_load ?(name = "") v = build_load v name builder

    (* Aliases *)
    let const_int = Llvm.const_int

    let const_bool lltype v =
      match v with
      | true -> const_int lltype 1
      | false -> const_int lltype 0
    ;;

    let params = Llvm.params

    let lookup_func_exn fname =
      match lookup_function fname module_ with
      | Some f -> f
      | None -> failwith (sprintf "Function '%s' not found" fname)
    ;;
  end
  in
  (module L : S)
;;
