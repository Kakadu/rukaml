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
       3	extern rukaml_print_int
       4	extern rukaml_applyN
       5	
       6	_start:
       7	              push    rbp
       8	              mov     rbp, rsp   ; prologue
       9	              call main
      10	              mov rdi, rax    ; rdi stores return code
      11	              mov rax, 60     ; exit syscall
      12	              syscall
      13	
      14		; @[{stack||stack}@]
      15	GLOBAL sq
      16	
      17	sq:
      18	  push rbp
      19	  mov  rbp, rsp
      20	  sub rsp, 8 ; allocate for var "__temp3"
      21	  mov rdx, [rsp+3*8] 
      22	  mov [rsp], rdx ; access a var "y"
      23	  sub rsp, 8 ; allocate for var "__temp4"
      24	  mov rdx, [rsp+4*8] 
      25	  mov [rsp], rdx ; access a var "y"
      26	  mov rax, [8*1+rsp]
      27	  mov rbx, [rsp]
      28	  imul rbx, rax
      29	  mov rax, rbx
      30	  add rsp, 8 ; deallocate var "__temp4"
      31	  add rsp, 8 ; deallocate var "__temp3"
      32	  pop rbp
      33	  ret  ;;;; sq
      34	
      35		; @[{stack||stack}@]
      36	GLOBAL double
      37	double:
      38	  push rbp
      39	  mov  rbp, rsp
      40	  sub rsp, 8 ; allocate for var "__temp7"
      41	  mov rdx, [rsp+3*8] 
      42	  mov [rsp], rdx ; access a var "x"
      43	  sub rsp, 8 ; allocate for var "__temp8"
      44	  mov rdx, [rsp+4*8] 
      45	  mov [rsp], rdx ; access a var "x"
      46	  mov rax, [8*1+rsp]
      47	  mov rbx, [rsp]
      48	  add  rbx, rax
      49	  mov rax, rbx
      50	  add rsp, 8 ; deallocate var "__temp8"
      51	  add rsp, 8 ; deallocate var "__temp7"
      52	  pop rbp
      53	  ret  ;;;; double
      54	
      55		; @[{stack||stack}@]
      56	GLOBAL main
      57	main:
      58	  push rbp
      59	  mov  rbp, rsp
      60		; expected_arity = 1
      61		; formal_arity = 1
      62		; calling "sq"
      63	  ; expected_arity = formal_arity = 1
      64	  sub rsp, 8 ; allocate for argument 0 (name = __temp11)
      65	  mov qword [rsp],  7
      66	  call sq
      67	  add rsp, 8 ; deallocate var "__temp11"
      68	  mov rax, rax
      69	  pop rbp
      70	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  [49]
