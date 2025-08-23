section .note.GNU-stack noalloc noexec nowrite progbits
section .text
extern rukaml_alloc_closure
extern rukaml_print_int
extern rukaml_applyN
extern rukaml_field
extern rukaml_alloc_pair
extern rukaml_initialize
extern rukaml_gc_compact
extern rukaml_gc_print_stats
extern rukaml_print_alloc_closure_count

_start:
              push    rbp
              mov     rbp, rsp   ; prologue
              call main
              mov rdi, rax    ; rdi stores return code
              mov rax, 60     ; exit syscall
              syscall
GLOBAL fac

fac:
  push rbp
  mov  rbp, rsp
  sub rsp, 8*3 ; allocate for local variables p2, p, temp1
  sub rsp, 8 ; allocate padding for locals
  mov qword r11, [rbp+2*8]
  mov qword r12, 1
  cmp r11, r12
  je lab_9
  mov qword [rbp-1*8], 0
  jmp lab_10
lab_9:
  mov qword [rbp-1*8], 1
  jmp lab_10
lab_10:
  mov qword rdx, [rbp-1*8]
  cmp rdx, 0
  je lab_then_11
  mov qword rax,  1
  jmp lab_endif_12
lab_then_11:
  mov qword r11, [rbp+2*8]
  dec r11
  mov qword [rbp-2*8], r11
  sub rsp, 8 ; trying to save alignment 16 bytes
  sub rsp, 8*1 ; fun arguments
  mov qword r8, [rbp-2*8]  ; arg "p"
  mov qword [rsp+0*8], r8
  call fac
  add rsp, 8*2 ; dealloc args
  mov [rbp-3*8], rax
  mov qword r11, [rbp+2*8]
  mov qword r12, [rbp-3*8]
  imul r11, r12
  mov rax, r11
lab_endif_12:
  add rsp, 8 ; deallocate padding for locals
  add rsp, 8*3 ; deallocate local variables p2, p, temp1
  pop rbp
  ret  ;;;; fac
GLOBAL main
main:
  push rbp
  mov  rbp, rsp
  mov rdi, rsp
  call rukaml_initialize
  sub rsp, 8*2 ; allocate for local variables t, n
  sub rsp, 8 ; trying to save alignment 16 bytes
  sub rsp, 8*1 ; fun arguments
  mov qword [rsp+0*8], 4 ; constant
  call fac
  add rsp, 8*2 ; dealloc args
  mov [rbp-1*8], rax
  add rsp, -8*2
  mov r11, [rbp-1*8]
  mov qword [rsp], r11
  call rukaml_print_int ; short
  add rsp, 8*2
  mov [rbp-2*8], rax
  mov qword rax,  0
  add rsp, 8*2 ; deallocate local variables t, n
  pop rbp
  ret  ;;;; main
