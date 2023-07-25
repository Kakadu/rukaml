section .text
extern rukaml_alloc_closure
extern rukaml_print_int
extern rukaml_applyN

_start:
              push    rbp
              mov     rbp, rsp   ; prologue
              call main
              mov rdi, rax    ; rdi stores return code
              mov rax, 60     ; exit syscall
              syscall

	; @[{stack||stack}@]
GLOBAL fac

fac:
  push rbp
  mov  rbp, rsp
  sub rsp, 8 ; allocate for var "temp1"
  sub rsp, 8 ; allocate for var "__temp3"
  mov rdx, [rsp+5*8] 
  mov [rsp], rdx ; access a var "n"
  sub rsp, 8 ; allocate for var "__temp4"
  mov qword [rsp],  1
  mov rax, [8*1+rsp]
  mov rbx, [rsp]
  cmp rax, rbx
  je lab_17
  mov qword [8*2+rsp], 0
  jmp lab_18
  lab_17:
    mov qword [8*2+rsp], 1
    jmp lab_18
  lab_18:
  add rsp, 8 ; deallocate var "__temp4"
  add rsp, 8 ; deallocate var "__temp3"
  mov rdx, [rsp+0*8] 
  cmp rdx, 0
  je lab_then_19
  mov qword rax,  1
  jmp lab_endif_20
  lab_then_19:
  sub rsp, 8 ; allocate for var "temp3"
  sub rsp, 8 ; allocate for var "__temp5"
  mov rdx, [rsp+6*8] 
  mov [rsp], rdx ; access a var "n"
  mov rax, [rsp]
  dec rax
  mov [8*1+rsp], rax
  add rsp, 8 ; deallocate var "__temp5"
  sub rsp, 8 ; allocate for var "temp4"
	; expected_arity = 2
	; formal_arity = 1
	; calling "fac"
  sub rsp, 8 ; allocate wrapper for func __temp6
  mov rdi, fac
  mov rsi, 2
  call rukaml_alloc_closure
  mov [rsp], rax
  sub rsp, 8 ; allocate for argument 0 (name = __temp7)
  mov rdx, [rsp+3*8] 
  mov [rsp], rdx ; access a var "temp3"
  mov rdi, [8*1+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  mov al, 0
  call rukaml_applyN
  mov [8*2+rsp], rax
  add rsp, 8 ; deallocate var "__temp7"
  add rsp, 8 ; deallocate var "__temp6"
  sub rsp, 8 ; allocate for var "temp5"
  sub rsp, 8 ; allocate for var "__temp8"
  mov rdx, [rsp+7*8] 
  mov [rsp], rdx ; access a var "u"
  mov rax, 0  ; no float arguments
  mov rdi, [8*2+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  call rukaml_applyN
  add rsp, 8 ; deallocate var "__temp8"
  mov [rsp], rax
  sub rsp, 8 ; allocate for var "__temp9"
  mov rdx, [rsp+1*8] 
  mov [rsp], rdx ; access a var "temp5"
  sub rsp, 8 ; allocate for var "__temp10"
  mov rdx, [rsp+9*8] 
  mov [rsp], rdx ; access a var "n"
  mov rax, [8*1+rsp]
  mov rbx, [rsp]
  imul rbx, rax
  mov rax, rbx
  add rsp, 8 ; deallocate var "__temp10"
  add rsp, 8 ; deallocate var "__temp9"
  add rsp, 8 ; deallocate var "temp5"
  add rsp, 8 ; deallocate var "temp4"
  add rsp, 8 ; deallocate var "temp3"
  lab_endif_20:
  add rsp, 8 ; deallocate var "temp1"
  pop rbp
  ret  ;;;; fac

	; @[{stack||stack}@]
GLOBAL loop
loop:
  push rbp
  mov  rbp, rsp
  sub rsp, 8 ; allocate for var "temp7"
  mov rdi, [8*4+rsp]
  call rukaml_print_int ; short
  mov [rsp], rax
  sub rsp, 8 ; allocate for var "t"
  mov rdx, [rsp+1*8] 
  mov [rsp], rdx ; access a var "temp7"
  sub rsp, 8 ; allocate for var "temp8"
  sub rsp, 8 ; allocate for var "__temp13"
  mov rdx, [rsp+7*8] 
  mov [rsp], rdx ; access a var "n"
  sub rsp, 8 ; allocate for var "__temp14"
  mov qword [rsp],  10
  mov rax, [8*1+rsp]
  mov rbx, [rsp]
  cmp rax, rbx
  je lab_21
  mov qword [8*2+rsp], 0
  jmp lab_22
  lab_21:
    mov qword [8*2+rsp], 1
    jmp lab_22
  lab_22:
  add rsp, 8 ; deallocate var "__temp14"
  add rsp, 8 ; deallocate var "__temp13"
  mov rdx, [rsp+0*8] 
  cmp rdx, 0
  je lab_then_23
  sub rsp, 8 ; allocate for var "temp13"
  sub rsp, 8 ; allocate for var "__temp15"
  mov qword [rsp],  53
  mov rdi, [rsp]
  call rukaml_print_int
  mov [8*1+rsp], rax
  add rsp, 8 ; deallocate var "__temp15"
  sub rsp, 8 ; allocate for var "v"
  mov rdx, [rsp+1*8] 
  mov [rsp], rdx ; access a var "temp13"
  mov qword rax,  1
  add rsp, 8 ; deallocate var "v"
  add rsp, 8 ; deallocate var "temp13"
  jmp lab_endif_24
  lab_then_23:
  sub rsp, 8 ; allocate for var "temp10"
  sub rsp, 8 ; allocate for var "__temp16"
  mov rdx, [rsp+8*8] 
  mov [rsp], rdx ; access a var "n"
  sub rsp, 8 ; allocate for var "__temp17"
  mov qword [rsp],  1
  mov rax, [8*1+rsp]
  mov rbx, [rsp]
  add  rbx, rax
  mov [8*2+rsp], rbx
  add rsp, 8 ; deallocate var "__temp17"
  add rsp, 8 ; deallocate var "__temp16"
  sub rsp, 8 ; allocate for var "temp11"
	; expected_arity = 2
	; formal_arity = 1
	; calling "loop"
  sub rsp, 8 ; allocate wrapper for func __temp18
  mov rdi, loop
  mov rsi, 2
  call rukaml_alloc_closure
  mov [rsp], rax
  sub rsp, 8 ; allocate for argument 0 (name = __temp19)
  mov rdx, [rsp+3*8] 
  mov [rsp], rdx ; access a var "temp10"
  mov rdi, [8*1+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  mov al, 0
  call rukaml_applyN
  mov [8*2+rsp], rax
  add rsp, 8 ; deallocate var "__temp19"
  add rsp, 8 ; deallocate var "__temp18"
  sub rsp, 8 ; allocate for var "__temp20"
  mov rdx, [rsp+8*8] 
  mov [rsp], rdx ; access a var "j"
  mov rax, 0  ; no float arguments
  mov rdi, [8*1+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  call rukaml_applyN
  add rsp, 8 ; deallocate var "__temp20"
  mov rax, rax
  add rsp, 8 ; deallocate var "temp11"
  add rsp, 8 ; deallocate var "temp10"
  lab_endif_24:
  add rsp, 8 ; deallocate var "temp8"
  add rsp, 8 ; deallocate var "t"
  add rsp, 8 ; deallocate var "temp7"
  pop rbp
  ret  ;;;; loop

	; @[{stack||stack}@]
GLOBAL main
main:
  push rbp
  mov  rbp, rsp
  sub rsp, 8 ; allocate for var "temp14"
	; expected_arity = 2
	; formal_arity = 1
	; calling "loop"
  sub rsp, 8 ; allocate wrapper for func __temp23
  mov rdi, loop
  mov rsi, 2
  call rukaml_alloc_closure
  mov [rsp], rax
  sub rsp, 8 ; allocate for argument 0 (name = __temp24)
  mov qword [rsp],  0
  mov rdi, [8*1+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  mov al, 0
  call rukaml_applyN
  mov [8*2+rsp], rax
  add rsp, 8 ; deallocate var "__temp24"
  add rsp, 8 ; deallocate var "__temp23"
  sub rsp, 8 ; allocate for var "temp15"
  sub rsp, 8 ; allocate for var "__temp25"
  mov qword [rsp],  12
  mov rax, 0  ; no float arguments
  mov rdi, [8*2+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  call rukaml_applyN
  add rsp, 8 ; deallocate var "__temp25"
  mov [rsp], rax
  sub rsp, 8 ; allocate for var "t44"
  mov rdx, [rsp+1*8] 
  mov [rsp], rdx ; access a var "temp15"
  sub rsp, 8 ; allocate for var "temp16"
  mov rdi, [8*1+rsp]
  call rukaml_print_int ; short
  mov [rsp], rax
  sub rsp, 8 ; allocate for var "tmppppppp"
  mov rdx, [rsp+1*8] 
  mov [rsp], rdx ; access a var "temp16"
  mov qword rax,  0
  add rsp, 8 ; deallocate var "tmppppppp"
  add rsp, 8 ; deallocate var "temp16"
  add rsp, 8 ; deallocate var "t44"
  add rsp, 8 ; deallocate var "temp15"
  add rsp, 8 ; deallocate var "temp14"
  pop rbp
  ret  ;;;; main
