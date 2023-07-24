section .text
extern rukaml_alloc_closure
extern rukaml_applyN

_start:
              push    rbp
              mov     rbp, rsp   ; prologue
              call main
              mov rdi, rax    ; rdi stores return code
              mov rax, 60     ; exit syscall
              syscall

	; @[{stack||stack}@]
GLOBAL prod

prod:
  push rbp
  mov  rbp, rsp
  sub rsp, 8 ; allocate for var "__temp3"
  mov rdx, [rsp+4*8] 
  mov [rsp], rdx ; access a var "a"
  sub rsp, 8 ; allocate for var "__temp4"
  mov rdx, [rsp+4*8] 
  mov [rsp], rdx ; access a var "b"
  mov rax, [8*1+rsp]
  mov rbx, [rsp]
  imul rbx, rax
  mov rax, rbx
  add rsp, 8 ; deallocate var "__temp4"
  add rsp, 8 ; deallocate var "__temp3"
  pop rbp
  ret  ;;;; prod

	; @[{stack||stack}@]
GLOBAL sum
sum:
  push rbp
  mov  rbp, rsp
  sub rsp, 8 ; allocate for var "__temp7"
  mov rdx, [rsp+4*8] 
  mov [rsp], rdx ; access a var "a"
  sub rsp, 8 ; allocate for var "__temp8"
  mov rdx, [rsp+4*8] 
  mov [rsp], rdx ; access a var "b"
  mov rax, [8*1+rsp]
  mov rbx, [rsp]
  add  rbx, rax
  mov rax, rbx
  add rsp, 8 ; deallocate var "__temp8"
  add rsp, 8 ; deallocate var "__temp7"
  pop rbp
  ret  ;;;; sum

	; @[{stack||stack}@]
GLOBAL main
main:
  push rbp
  mov  rbp, rsp
  sub rsp, 8 ; allocate for var "temp3"
  sub rsp, 8 ; allocate for var "__temp11"
  mov qword [rsp],  1
  sub rsp, 8 ; allocate for var "__temp12"
  mov qword [rsp],  1
  mov rax, [8*1+rsp]
  mov rbx, [rsp]
  cmp rax, rbx
  je lab_7
  mov qword [8*2+rsp], 0
  jmp lab_8
  lab_7:
    mov qword [8*2+rsp], 1
    jmp lab_8
  lab_8:
  add rsp, 8 ; deallocate var "__temp12"
  add rsp, 8 ; deallocate var "__temp11"
  sub rsp, 8 ; allocate for var "temp4"
  mov rdx, [rsp+1*8] 
  cmp rdx, 0
  je lab_then_9
  mov rdi, sum
  mov rsi, 2
  call rukaml_alloc_closure
  mov [rsp], rax
  jmp lab_endif_10
  lab_then_9:
  mov rdi, prod
  mov rsi, 2
  call rukaml_alloc_closure
  mov [rsp], rax
  lab_endif_10:
  sub rsp, 8 ; allocate for var "temp5"
  sub rsp, 8 ; allocate for var "__temp13"
  mov qword [rsp],  7
  mov rax, 0  ; no float arguments
  mov rdi, [8*2+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  call rukaml_applyN
  add rsp, 8 ; deallocate var "__temp13"
  mov [rsp], rax
  sub rsp, 8 ; allocate for var "__temp14"
  mov qword [rsp],  13
  mov rax, 0  ; no float arguments
  mov rdi, [8*1+rsp]
  mov rsi, 1
  mov rdx, [rsp]
  call rukaml_applyN
  add rsp, 8 ; deallocate var "__temp14"
  mov rax, rax
  add rsp, 8 ; deallocate var "temp5"
  add rsp, 8 ; deallocate var "temp4"
  add rsp, 8 ; deallocate var "temp3"
  pop rbp
  ret  ;;;; main
