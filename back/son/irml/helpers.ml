open Bindings
let size_buf () = Ctypes.allocate Ctypes.ulong Unsigned.ULong.zero

module Code_buf : sig
  type t
  val create : int -> t
  val add_thunk : t -> unit Ctypes_static.ptr * Unsigned.ULong.t
  val fix_thunk : t -> unit Ctypes_static.ptr -> unit Ctypes_static.ptr -> unit
end = struct
  (* buf ptr, start ptr, size *)
  type t =
    (code_buffer, [ `Struct ]) Ctypes.structured Ctypes.ptr
    * unit Ctypes.ptr
    * Unsigned.ULong.t
  let create size =
    let open Ctypes in
    let buf = make code_buffer in
    let sz_sizet = Unsigned.ULong.of_int size in
    let st =
      (*TODO: err hndl?*)
      ir_mem_mmap sz_sizet
    in
    setf buf start st;
    setf buf endd @@ to_voidp (from_voidp (ptr char) st +@ size);
    setf buf pos st;
    addr buf, st, sz_sizet
  ;;

  let add_thunk (buf, st, size) =
    let open Ctypes in
    let sz_buf = size_buf () in
    (*TODO: err hndl?*)
    ignore @@ ir_mem_unprotect st size;
    (*TODO: err hndl?*)
    let thunk = ir_emit_thunk buf null sz_buf in
    (*TODO: err hndl?*)
    ignore @@ ir_mem_protect st size;
    thunk, !@sz_buf
  ;;

  let fix_thunk (_, st, size) th new_addr =
    (*TODO: err hndl?*)
    ignore @@ ir_mem_unprotect st size;
    ir_fix_thunk th new_addr;
    (*TODO: err hndl?*)
    ignore @@ ir_mem_protect st size
  ;;
end
