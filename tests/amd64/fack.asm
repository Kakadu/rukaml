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
GLOBAL fresh_1

fresh_1:
  push rbp
  mov  rbp, rsp
  sub rsp, 8 ; allocate for var "temp1"
  sub rsp, 8 ; allocate for var "__temp3"
  mov rdx, [rsp+6*8] 
  mov [rsp], rdx ; access a var "u"
  sub rsp, 8 ; allocate for var "__temp4"
  mov rdx, [rsp+5*8] 
  mov [rsp], rdx ; access a var "n"
  mov rax, [8*1+rsp]
  mov rbx, [rsp]
  imul rbx, rax
  mov [8*2+rsp], rbx
  add rsp, 8 ; deallocate var "__temp4"
  add rsp, 8 ; deallocate var "__temp3"
  sub rsp, 8 ; allocate for var "__temp5"
  mov rdx, [rsp+1*8] 
  mov [rsp], rdx ; access a var "temp1"
  mov rax, 0  ; no float arguments
  mov rdi, [8*5+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  call rukaml_applyN
  add rsp, 8 ; deallocate var "__temp5"
  mov rax, rax
  add rsp, 8 ; deallocate var "temp1"
  pop rbp
  ret  ;;;; fresh_1

	; @[{stack||stack}@]
GLOBAL fac
fac:
  push rbp
  mov  rbp, rsp
  sub rsp, 8 ; allocate for var "temp3"
  sub rsp, 8 ; allocate for var "__temp8"
  mov rdx, [rsp+4*8] 
  mov [rsp], rdx ; access a var "n"
  sub rsp, 8 ; allocate for var "__temp9"
  mov qword [rsp],  1
  mov rax, [8*1+rsp]
  mov rbx, [rsp]
  cmp rax, rbx
  je lab_14
  mov qword [8*2+rsp], 0
  jmp lab_15
  lab_14:
    mov qword [8*2+rsp], 1
    jmp lab_15
  lab_15:
  add rsp, 8 ; deallocate var "__temp9"
  add rsp, 8 ; deallocate var "__temp8"
  mov rdx, [rsp+0*8] 
  cmp rdx, 0
  je lab_then_16
  sub rsp, 8 ; allocate for var "__temp10"
  mov qword [rsp],  1
  mov rax, 0  ; no float arguments
  mov rdi, [8*5+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  call rukaml_applyN
  add rsp, 8 ; deallocate var "__temp10"
  mov rax, rax
  jmp lab_endif_17
  lab_then_16:
  sub rsp, 8 ; allocate for var "temp5"
  sub rsp, 8 ; allocate for var "__temp11"
  mov rdx, [rsp+5*8] 
  mov [rsp], rdx ; access a var "n"
  mov rax, [rsp]
  dec rax
  mov [8*1+rsp], rax
  add rsp, 8 ; deallocate var "__temp11"
  sub rsp, 8 ; allocate for var "temp6"
	; expected_arity = 2
	; formal_arity = 1
	; calling "fac"
  sub rsp, 8 ; allocate wrapper for func __temp12
  mov rdi, fac
  mov rsi, 2
  call rukaml_alloc_closure
  mov [rsp], rax
  sub rsp, 8 ; allocate for argument 0 (name = __temp13)
  mov rdx, [rsp+3*8] 
  mov [rsp], rdx ; access a var "temp5"
  mov rdi, [8*1+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  mov al, 0
  call rukaml_applyN
  mov [8*2+rsp], rax
  add rsp, 8 ; deallocate var "__temp13"
  add rsp, 8 ; deallocate var "__temp12"
  sub rsp, 8 ; allocate for var "temp7"
	; expected_arity = 3
	; formal_arity = 1
	; calling "fresh_1"
  sub rsp, 8 ; allocate wrapper for func __temp14
  mov rdi, fresh_1
  mov rsi, 3
  call rukaml_alloc_closure
  mov [rsp], rax
  sub rsp, 8 ; allocate for argument 0 (name = __temp15)
  mov rdx, [rsp+8*8] 
  mov [rsp], rdx ; access a var "n"
  mov rdi, [8*1+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  mov al, 0
  call rukaml_applyN
  mov [8*2+rsp], rax
  add rsp, 8 ; deallocate var "__temp15"
  add rsp, 8 ; deallocate var "__temp14"
  sub rsp, 8 ; allocate for var "temp8"
  sub rsp, 8 ; allocate for var "__temp16"
  mov rdx, [rsp+9*8] 
  mov [rsp], rdx ; access a var "k"
  mov rax, 0  ; no float arguments
  mov rdi, [8*2+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  call rukaml_applyN
  add rsp, 8 ; deallocate var "__temp16"
  mov [rsp], rax
  sub rsp, 8 ; allocate for var "__temp17"
  mov rdx, [rsp+1*8] 
  mov [rsp], rdx ; access a var "temp8"
  mov rax, 0  ; no float arguments
  mov rdi, [8*3+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  call rukaml_applyN
  add rsp, 8 ; deallocate var "__temp17"
  mov rax, rax
  add rsp, 8 ; deallocate var "temp8"
  add rsp, 8 ; deallocate var "temp7"
  add rsp, 8 ; deallocate var "temp6"
  add rsp, 8 ; deallocate var "temp5"
  lab_endif_17:
  add rsp, 8 ; deallocate var "temp3"
  pop rbp
  ret  ;;;; fac

	; @[{stack||stack}@]
GLOBAL id
id:
  push rbp
  mov  rbp, rsp
  mov rdx, [rsp+2*8] 
  mov rax, rdx ; access a var "u"
  pop rbp
  ret  ;;;; id

	; @[{stack||stack}@]
GLOBAL main
main:
  push rbp
  mov  rbp, rsp
  sub rsp, 8 ; allocate for var "temp11"
	; expected_arity = 2
	; formal_arity = 1
	; calling "fac"
  sub rsp, 8 ; allocate wrapper for func __temp22
  mov rdi, fac
  mov rsi, 2
  call rukaml_alloc_closure
  mov [rsp], rax
  sub rsp, 8 ; allocate for argument 0 (name = __temp23)
  mov qword [rsp],  4
  mov rdi, [8*1+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  mov al, 0
  call rukaml_applyN
  mov [8*2+rsp], rax
  add rsp, 8 ; deallocate var "__temp23"
  add rsp, 8 ; deallocate var "__temp22"
  sub rsp, 8 ; allocate for var "temp12"
  sub rsp, 8 ; allocate for var "__temp24"
  mov rdi, id
  mov rsi, 1
  call rukaml_alloc_closure
  mov [rsp], rax
  mov rax, 0  ; no float arguments
  mov rdi, [8*2+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  call rukaml_applyN
  add rsp, 8 ; deallocate var "__temp24"
  mov [rsp], rax
  mov rdi, [rsp]
  call rukaml_print_int ; short
  mov rax, rax
  add rsp, 8 ; deallocate var "temp12"
  add rsp, 8 ; deallocate var "temp11"
  pop rbp
  ret  ;;;; main
