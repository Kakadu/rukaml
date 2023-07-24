  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  > let sq y = y * y
  > let double x = x+x
  > let main = sq 7
  > EOF
  After ANF transformation.
  let sq y =
    (y * y)
  let double x =
    (x + x)
  let main =
    sq 7 
  ANF: let sq y =
         (y * y)
       let double x =
         (x + x)
       let main =
         sq 7 

; generated code for amd64
  $ cat program.asm | nl -ba
       1	section .text
       2	extern rukaml_alloc_closure
       3	extern rukaml_applyN
       4	
       5	_start:
       6	              push    rbp
       7	              mov     rbp, rsp   ; prologue
       8	              call main
       9	              mov rdi, rax    ; rdi stores return code
      10	              mov rax, 60     ; exit syscall
      11	              syscall
      12	
      13		; @[{stack||stack}@]
      14	GLOBAL sq
      15	
      16	sq:
      17	  push rbp
      18	  mov  rbp, rsp
      19	  sub rsp, 8 ; allocate for var "__temp3"
      20	  mov rdx, [rsp+3*8] 
      21	  mov [rsp], rdx ; access a var "y"
      22	  sub rsp, 8 ; allocate for var "__temp4"
      23	  mov rdx, [rsp+4*8] 
      24	  mov [rsp], rdx ; access a var "y"
      25	  mov rax, [8*1+rsp]
      26	  mov rbx, [rsp]
      27	  imul rbx, rax
      28	  mov rax, rbx
      29	  add rsp, 8 ; deallocate var "__temp4"
      30	  add rsp, 8 ; deallocate var "__temp3"
      31	  pop rbp
      32	  ret  ;;;; sq
      33	
      34		; @[{stack||stack}@]
      35	GLOBAL double
      36	double:
      37	  push rbp
      38	  mov  rbp, rsp
      39	  sub rsp, 8 ; allocate for var "__temp7"
      40	  mov rdx, [rsp+3*8] 
      41	  mov [rsp], rdx ; access a var "x"
      42	  sub rsp, 8 ; allocate for var "__temp8"
      43	  mov rdx, [rsp+4*8] 
      44	  mov [rsp], rdx ; access a var "x"
      45	  mov rax, [8*1+rsp]
      46	  mov rbx, [rsp]
      47	  add  rbx, rax
      48	  mov rax, rbx
      49	  add rsp, 8 ; deallocate var "__temp8"
      50	  add rsp, 8 ; deallocate var "__temp7"
      51	  pop rbp
      52	  ret  ;;;; double
      53	
      54		; @[{stack||stack}@]
      55	GLOBAL main
      56	main:
      57	  push rbp
      58	  mov  rbp, rsp
      59		; expected_arity = 1
      60		; formal_arity = 1
      61		; calling "sq"
      62	  ; expected_arity = formal_arity = 1
      63	  sub rsp, 8 ; allocate for argument 0 (name = __temp11)
      64	  mov qword [rsp],  7
      65	  call sq
      66	  add rsp, 8 ; deallocate var "__temp11"
      67	  mov rax, rax
      68	  pop rbp
      69	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  [49]
