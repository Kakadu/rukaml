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

_start:
              push    rbp
              mov     rbp, rsp   ; prologue
              call main
              mov rdi, rax    ; rdi stores return code
              mov rax, 60     ; exit syscall
              syscall
GLOBAL main

main:
  push rbp
  mov  rbp, rsp
  mov rdi, rsp
  call rukaml_initialize
  sub rsp, 8*1 ; allocate for local variables temp1
  sub rsp, 8 ; allocate padding for locals
    ;; calculate rhs and put into [rbp-1*8]. offset = 1
  mov qword [rbp-1*8], 1
  mov qword rdx, [rbp-1*8]
  cmp rdx, 0
  je lab_then_4
  mov qword [rbp-1*8],  10
  jmp lab_endif_5
lab_then_4:
  mov qword [rbp-1*8],  20
lab_endif_5:
  add rsp, 8 ; deallocate padding for locals
  add rsp, 8*1 ; deallocate local variables temp1
  pop rbp
  ret  ;;;; main
