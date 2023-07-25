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
  je lab_10
  mov qword [8*2+rsp], 0
  jmp lab_11
  lab_10:
    mov qword [8*2+rsp], 1
    jmp lab_11
  lab_11:
  add rsp, 8 ; deallocate var "__temp4"
  add rsp, 8 ; deallocate var "__temp3"
  mov rdx, [rsp+0*8]
  cmp rdx, 0
  je lab_then_12
  mov qword rax,  1
  jmp lab_endif_13
  lab_then_12:
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
  lab_endif_13:
  add rsp, 8 ; deallocate var "temp1"
  pop rbp
  ret  ;;;; fac

	; @[{stack||stack}@]
GLOBAL main
main:
  push rbp
  mov  rbp, rsp
  sub rsp, 8 ; allocate for var "temp7"
	; expected_arity = 2
	; formal_arity = 1
	; calling "fac"
  mov rdi, fac
  mov rsi, 2
  call rukaml_alloc_closure

  sub rsp, 8 ; allocate wrapper for func __temp13
  mov [rsp], rax
  mov qword [rsp],  5
  mov rdi, [8*1+rsp]
  mov rsi, 1
  mov rdx, 5
  mov al, 0
  call rukaml_applyN
	mov [8*1+rsp], rax

  add rsp, 8 ; deallocate var "__temp13"
  sub rsp, 8 ; allocate for var "temp8"
  mov rax, 0  ; no float arguments
  mov rdi, [8*2+rsp]
  mov rsi, 1
  mov rdx, 666
  call rukaml_applyN
  mov [rsp], rax
  
	sub rsp, 8 ; allocate for var "t44"
  mov [rsp], [rsp+8] ; access a var "temp8"
  mov rdi, [8+rsp]
  call rukaml_print_int ; short
  mov qword rax,  0
  add rsp, 8 ; deallocate var "t44"
  add rsp, 8 ; deallocate var "temp8"
  add rsp, 8 ; deallocate var "temp7"
  pop rbp
  ret  ;;;; main
