(* Hack needed to make symbols available, see constfun's comment here
 * https://github.com/ocamllabs/ocaml-ctypes/issues/541 *)
external _force_link_ : unit -> unit = "ir_regs_number"
external _force_link_ : unit -> unit = "ir_consistency_check"
external _force_link_ : unit -> unit = "ir_disasm_add_symbol_w"
external _force_link_ : unit -> unit = "ir_sccp"
external _force_link_ : unit -> unit = "ir_save"

open Ctypes

open Foreign

(* some stdio.h things *)

type file_ptr = unit ptr
let file_ptr : file_ptr typ = ptr void

let fopen = foreign "fopen" (string @-> string @-> returning file_ptr)
let fclose = foreign "fclose" (file_ptr @-> returning int)
let fprintf = foreign "fprintf" (file_ptr @-> string @-> returning int)

(* SoN *)

type ir_ctx_ptr = unit ptr

let ir_ctx_ptr : ir_ctx_ptr typ = ptr void

type code_buffer
let code_buffer : code_buffer structure typ = structure "code_buffer"

let start = field code_buffer "start" @@ ptr void
let endd = field code_buffer "end" @@ ptr void

let pos = field code_buffer "pos" @@ ptr void;;

seal code_buffer

(*TODO: due definitions from ir.h as comments? *)
let ir_param =
  foreign
    "ir_param"
    (ir_ctx_ptr @-> uint16_t @-> int32_t @-> string @-> int @-> returning int32_t)
;;
let ir_const_i64 = foreign "ir_const_i64" (ir_ctx_ptr @-> int64_t @-> returning int32_t)

let ir_const_addr_w =
  foreign "ir_const_addr_w" (ir_ctx_ptr @-> ptr void @-> returning int32_t)
;;

let ir_fold1 =
  foreign "ir_fold1" (ir_ctx_ptr @-> uint32_t @-> int32_t @-> returning int32_t)
;;
let ir_fold2 =
  foreign
    "ir_fold2"
    (ir_ctx_ptr @-> uint32_t @-> int32_t @-> int32_t @-> returning int32_t)
;;
let ir_emit0 = foreign "ir_emit0" (ir_ctx_ptr @-> uint32_t @-> returning int32_t)
let ir_emit1 =
  foreign "ir_emit1" (ir_ctx_ptr @-> uint32_t @-> int32_t @-> returning int32_t)
;;
let ir_emit2 =
  foreign
    "ir_emit2"
    (ir_ctx_ptr @-> uint32_t @-> int32_t @-> int32_t @-> returning int32_t)
;;
let ir_emit3 =
  foreign
    "ir_emit3"
    (ir_ctx_ptr @-> uint32_t @-> int32_t @-> int32_t @-> int32_t @-> returning int32_t)
;;
let ir_emitN =
  foreign "ir_emit_N" (ir_ctx_ptr @-> uint32_t @-> int32_t @-> returning int32_t)
;;
let ir_set_op =
  foreign "ir_set_op" (ir_ctx_ptr @-> int32_t @-> int32_t @-> int32_t @-> returning void)
;;
let ir_create_ctx = foreign "ir_create_ctx" (void @-> returning ir_ctx_ptr)
let ir_consistency_check = foreign "ir_consistency_check" (void @-> returning void)
let ir_init =
  foreign "ir_init" (ir_ctx_ptr @-> uint32_t @-> int32_t @-> int32_t @-> returning void)
;;
let ir_build_cfg = foreign "ir_build_cfg" (ir_ctx_ptr @-> returning void)
let ir_build_dominators_tree =
  foreign "ir_build_dominators_tree" (ir_ctx_ptr @-> returning void)
;;
let ir_build_def_use_lists =
  foreign "ir_build_def_use_lists" (ir_ctx_ptr @-> returning void)
;;
let ir_sccp = foreign "ir_sccp" (ir_ctx_ptr @-> returning int)
let ir_find_loops = foreign "ir_find_loops" (ir_ctx_ptr @-> returning void)
let ir_gcm = foreign "ir_gcm" (ir_ctx_ptr @-> returning void)
let ir_schedule = foreign "ir_schedule" (ir_ctx_ptr @-> returning void)
let ir_match = foreign "ir_match" (ir_ctx_ptr @-> returning void)
let ir_assign_virtual_registers =
  foreign "ir_assign_virtual_registers" (ir_ctx_ptr @-> returning void)
;;
let ir_compute_live_ranges =
  foreign "ir_compute_live_ranges" (ir_ctx_ptr @-> returning void)
;;
let ir_coalesce = foreign "ir_coalesce" (ir_ctx_ptr @-> returning void)
let ir_reg_alloc = foreign "ir_reg_alloc" (ir_ctx_ptr @-> returning void)
let ir_schedule_blocks = foreign "ir_schedule_blocks" (ir_ctx_ptr @-> returning void)
let ir_emit_code =
  foreign "ir_emit_code" (ir_ctx_ptr @-> ptr ulong @-> returning @@ ptr void)
;;
let ir_disasm_init = foreign "ir_disasm_init" (void @-> returning int)
let ir_disasm_add_symbol_w =
  foreign "ir_disasm_add_symbol_w" (string @-> ptr void @-> ulong @-> returning void)
;;
let ir_disasm =
  foreign
    "ir_disasm"
    (const string
     @-> const (ptr void)
     @-> ulong
     @-> bool
     @-> ir_ctx_ptr
     @-> file_ptr
     @-> returning int)
;;
let ir_disasm_free = foreign "ir_disasm_free" (void @-> returning void)
let ir_free = foreign "ir_free" (ir_ctx_ptr @-> returning void)
let ir_save = foreign "ir_save" (ir_ctx_ptr @-> uint32_t @-> file_ptr @-> returning void)
let ir_mem_mmap = foreign "ir_mem_mmap" (ulong @-> returning (ptr void))
let ir_mem_unprotect = foreign "ir_mem_unprotect" (ptr void @-> ulong @-> returning int)
let ir_mem_protect = foreign "ir_mem_protect" (ptr void @-> ulong @-> returning int)
let ir_emit_thunk =
  foreign
    "ir_emit_thunk"
    (ptr code_buffer @-> ptr void @-> ptr ulong @-> returning @@ ptr void)
;;
let ir_fix_thunk = foreign "ir_fix_thunk" (ptr void @-> ptr void @-> returning void)
