open Llvm
open Printf

module type S = sig
  val context : Llvm.llcontext
  val module_ : Llvm.llmodule
  val builder : Llvm.llbuilder
  val build_store : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
  val build_call : ?name:string -> lltype -> llvalue -> llvalue list -> llvalue
  val lookup_func_exn : string -> llvalue
  val has_toplevel_func : string -> bool
  val build_add : ?name:string -> llvalue -> llvalue -> llvalue
  val build_sub : ?name:string -> llvalue -> llvalue -> llvalue
  val build_mul : ?name:string -> llvalue -> llvalue -> llvalue
  val build_sdiv : ?name:string -> llvalue -> llvalue -> llvalue
  val build_icmp : ?name:string -> Icmp.t -> llvalue -> llvalue -> llvalue

  (** [set_metadata v kind fmt] sets metadata to value [v] of kind [k].
      Returns this value [v]. Useful for attaching debugging *)
  val set_metadata
    :  llvalue
    -> string
    -> ('a, Format.formatter, unit, llvalue) format4
    -> 'a

  (* ?? *)

  val build_ptrtoint : ?name:string -> llvalue -> lltype -> llvalue
  val build_inttoptr : ?name:string -> llvalue -> lltype -> llvalue
  val build_pointercast : ?name:string -> llvalue -> lltype -> llvalue

  (** Just aliases *)

  val const_int : Llvm.lltype -> int -> Llvm.llvalue
  val params : Llvm.llvalue -> Llvm.llvalue array
  val pp_value : Format.formatter -> llvalue -> unit
end

let make context builder module_ =
  let module L : S = struct
    let context = context
    let builder = builder
    let module_ = module_
    let build_store a b = Llvm.build_store a b builder

    let build_call ?(name = "") fntyp f args =
      build_call fntyp f (Array.of_list args) name builder
    ;;

    let has_toplevel_func fname =
      match lookup_function fname module_ with
      | Some _ -> true
      | None -> false
    ;;

    let lookup_func_exn fname =
      match lookup_function fname module_ with
      | Some f -> f
      | None -> failwith (sprintf "Function '%s' not found" fname)
    ;;

    let build_add ?(name = "") l r = build_add l r name builder
    let build_sub ?(name = "") l r = build_sub l r name builder
    let build_mul ?(name = "") l r = build_mul l r name builder
    let build_sdiv ?(name = "") l r = build_sdiv l r name builder
    let build_icmp ?(name = "") op l r = build_icmp op l r name builder
    let build_ptrtoint ?(name = "") e typ = Llvm.build_ptrtoint e typ name builder
    let build_inttoptr ?(name = "") e typ = Llvm.build_inttoptr e typ name builder
    let build_pointercast ?(name = "") f typ = Llvm.build_pointercast f typ name builder

    let set_metadata v kind fmt =
      Format.kasprintf
        (fun s ->
           Llvm.set_metadata v (Llvm.mdkind_id context kind) (Llvm.mdstring context s);
           v)
        fmt
    ;;

    (* Aliases *)
    let const_int = Llvm.const_int
    let params = Llvm.params
    let pp_value ppf x = Format.fprintf ppf "%s" (Llvm.string_of_llvalue x)
  end
  in
  (module L : S)
;;
