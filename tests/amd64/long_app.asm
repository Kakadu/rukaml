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
GLOBAL wrap

wrap:
  push rbp
  mov  rbp, rsp
  sub rsp, 8*1 ; allocate for local variables temp1
  sub rsp, 8 ; allocate padding for locals
  mov qword [rbp-1*8], 1
  mov qword rdx, [rbp-1*8]
  cmp rdx, 0
  je lab_then_38
  mov qword rdx, [rbp+2*8] ; use temp rdx to move from stack to stack
  mov qword rax, rdx ; access a var "f"
  jmp lab_endif_39
lab_then_38:
  mov qword rdx, [rbp+2*8] ; use temp rdx to move from stack to stack
  mov qword rax, rdx ; access a var "f"
lab_endif_39:
  add rsp, 8 ; deallocate padding for locals
  add rsp, 8*1 ; deallocate local variables temp1
  pop rbp
  ret  ;;;; wrap
GLOBAL test3
test3:
  push rbp
  mov  rbp, rsp
  sub rsp, 8*3 ; allocate for local variables c, b, a
  sub rsp, 8 ; allocate padding for locals
  mov rdi, [rbp+2*8]
  call rukaml_print_int ; short
  mov [rbp-1*8], rax
  mov rdi, [rbp+3*8]
  call rukaml_print_int ; short
  mov [rbp-2*8], rax
  mov rdi, [rbp+4*8]
  call rukaml_print_int ; short
  mov [rbp-3*8], rax
  mov qword rax,  0
  add rsp, 8 ; deallocate padding for locals
  add rsp, 8*3 ; deallocate local variables c, b, a
  pop rbp
  ret  ;;;; test3
GLOBAL test7
test7:
  push rbp
  mov  rbp, rsp
  sub rsp, 8*5 ; allocate for local variables temp10, temp9, temp8, temp7, temp6
  sub rsp, 8 ; allocate padding for locals
  mov qword r11, [rbp+2*8]
  mov qword r12, [rbp+3*8]
  add  r11, r12
  mov [rbp-1*8], r11
  mov qword r11, [rbp-1*8]
  mov qword r12, [rbp+4*8]
  add  r11, r12
  mov [rbp-2*8], r11
  mov qword r11, [rbp-2*8]
  mov qword r12, [rbp+5*8]
  add  r11, r12
  mov [rbp-3*8], r11
  mov qword r11, [rbp-3*8]
  mov qword r12, [rbp+6*8]
  add  r11, r12
  mov [rbp-4*8], r11
  mov qword r11, [rbp-4*8]
  mov qword r12, [rbp+7*8]
  add  r11, r12
  mov [rbp-5*8], r11
  mov qword r11, [rbp-5*8]
  mov qword r12, [rbp+8*8]
  add  r11, r12
  mov rax, r11
  add rsp, 8 ; deallocate padding for locals
  add rsp, 8*5 ; deallocate local variables temp10, temp9, temp8, temp7, temp6
  pop rbp
  ret  ;;;; test7
GLOBAL test10
test10:
  push rbp
  mov  rbp, rsp
  sub rsp, 8*8 ; allocate for local variables temp19, temp18, temp17, temp16, temp15, temp14, temp13, temp12
  mov qword r11, [rbp+2*8]
  mov qword r12, [rbp+3*8]
  add  r11, r12
  mov [rbp-1*8], r11
  mov qword r11, [rbp-1*8]
  mov qword r12, [rbp+4*8]
  add  r11, r12
  mov [rbp-2*8], r11
  mov qword r11, [rbp-2*8]
  mov qword r12, [rbp+5*8]
  add  r11, r12
  mov [rbp-3*8], r11
  mov qword r11, [rbp-3*8]
  mov qword r12, [rbp+6*8]
  add  r11, r12
  mov [rbp-4*8], r11
  mov qword r11, [rbp-4*8]
  mov qword r12, [rbp+7*8]
  add  r11, r12
  mov [rbp-5*8], r11
  mov qword r11, [rbp-5*8]
  mov qword r12, [rbp+8*8]
  add  r11, r12
  mov [rbp-6*8], r11
  mov qword r11, [rbp-6*8]
  mov qword r12, [rbp+9*8]
  add  r11, r12
  mov [rbp-7*8], r11
  mov qword r11, [rbp-7*8]
  mov qword r12, [rbp+10*8]
  add  r11, r12
  mov [rbp-8*8], r11
  mov qword r11, [rbp-8*8]
  mov qword r12, [rbp+11*8]
  add  r11, r12
  mov rax, r11
  add rsp, 8*8 ; deallocate local variables temp19, temp18, temp17, temp16, temp15, temp14, temp13, temp12
  pop rbp
  ret  ;;;; test10
GLOBAL main
main:
  push rbp
  mov  rbp, rsp
  mov rdi, rsp
  call rukaml_initialize
  sub rsp, 8*16 ; allocate for local variables temp2, temp35, temp34, temp33, temp1, temp31, temp30, temp29, temp28, temp27, temp26, temp25, temp24, temp23, temp22, temp21
  sub rsp, 8 ; trying to save alignment 16 bytes
  sub rsp, 8*1 ; fun arguments
  mov rdi, test10
  mov rsi, 10
  call rukaml_alloc_closure
  mov qword [rsp+0*8], rax ; arg "test10"
  call wrap
  add rsp, 8*2 ; dealloc args
  mov [rbp-1*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp21
  mov qword [rbp-18*8],  1
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-1*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp21"
  mov [rbp-2*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp22
  mov qword [rbp-18*8],  10
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-2*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp22"
  mov [rbp-3*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp23
  mov qword [rbp-18*8],  100
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-3*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp23"
  mov [rbp-4*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp24
  mov qword [rbp-18*8],  1000
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-4*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp24"
  mov [rbp-5*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp25
  mov qword [rbp-18*8],  10000
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-5*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp25"
  mov [rbp-6*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp26
  mov qword [rbp-18*8],  100000
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-6*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp26"
  mov [rbp-7*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp27
  mov qword [rbp-18*8],  1000000
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-7*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp27"
  mov [rbp-8*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp28
  mov qword [rbp-18*8],  10000000
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-8*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp28"
  mov [rbp-9*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp29
  mov qword [rbp-18*8],  100000000
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-9*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp29"
  mov [rbp-10*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp30
  mov qword [rbp-18*8],  1000000000
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-10*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp30"
  mov [rbp-11*8], rax
  mov rdi, [rbp-11*8]
  call rukaml_print_int ; short
  mov [rbp-12*8], rax
  sub rsp, 8 ; trying to save alignment 16 bytes
  sub rsp, 8*1 ; fun arguments
  mov rdi, test3
  mov rsi, 3
  call rukaml_alloc_closure
  mov qword [rsp+0*8], rax ; arg "test3"
  call wrap
  add rsp, 8*2 ; dealloc args
  mov [rbp-13*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp33
  mov qword [rbp-18*8],  1
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-13*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp33"
  mov [rbp-14*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp34
  mov qword [rbp-18*8],  10
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-14*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp34"
  mov [rbp-15*8], rax
  sub rsp, 8 ; padding
  sub rsp, 8 ; first arg of a function temp35
  mov qword [rbp-18*8],  10
  mov rax, 0  ; no float arguments
  mov rdi, [rbp-15*8]
  mov rsi, 1
  mov rdx, [rbp-18*8]
  call rukaml_applyN
  add rsp, 8*2 ; free space for args of function "temp35"
  mov [rbp-16*8], rax
  mov qword rax,  0
  add rsp, 8*16 ; deallocate local variables temp2, temp35, temp34, temp33, temp1, temp31, temp30, temp29, temp28, temp27, temp26, temp25, temp24, temp23, temp22, temp21
  pop rbp
  ret  ;;;; main
