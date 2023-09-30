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
  $ cat program.asm  | grep -v 'section .note.GNU-stack' | nl -ba
       1	section .text
       2	extern rukaml_alloc_closure
       3	extern rukaml_print_int
       4	extern rukaml_applyN
       5	extern rukaml_field
       6	extern rukaml_alloc_pair
       7	extern rukaml_initialize
       8	extern rukaml_gc_compact
       9	extern rukaml_gc_print_stats
      10	
      11	_start:
      12	              push    rbp
      13	              mov     rbp, rsp   ; prologue
      14	              call main
      15	              mov rdi, rax    ; rdi stores return code
      16	              mov rax, 60     ; exit syscall
      17	              syscall
      18	GLOBAL sq
      19	
      20	sq:
      21	  push rbp
      22	  mov  rbp, rsp
      23	  sub rsp, 8 ; allocate for var "__temp3"
      24	  mov rdx, [rsp+3*8] 
      25	  mov [rsp], rdx ; access a var "y"
      26	  sub rsp, 8 ; allocate for var "__temp4"
      27	  mov rdx, [rsp+4*8] 
      28	  mov [rsp], rdx ; access a var "y"
      29	  mov rax, [8*1+rsp]
      30	  mov rbx, [rsp]
      31	  imul rbx, rax
      32	  mov rax, rbx
      33	  add rsp, 8 ; deallocate var "__temp4"
      34	  add rsp, 8 ; deallocate var "__temp3"
      35	  pop rbp
      36	  ret  ;;;; sq
      37	GLOBAL double
      38	double:
      39	  push rbp
      40	  mov  rbp, rsp
      41	  sub rsp, 8 ; allocate for var "__temp7"
      42	  mov rdx, [rsp+3*8] 
      43	  mov [rsp], rdx ; access a var "x"
      44	  sub rsp, 8 ; allocate for var "__temp8"
      45	  mov rdx, [rsp+4*8] 
      46	  mov [rsp], rdx ; access a var "x"
      47	  mov rax, [8*1+rsp]
      48	  mov rbx, [rsp]
      49	  add  rbx, rax
      50	  mov rax, rbx
      51	  add rsp, 8 ; deallocate var "__temp8"
      52	  add rsp, 8 ; deallocate var "__temp7"
      53	  pop rbp
      54	  ret  ;;;; double
      55	GLOBAL main
      56	main:
      57	  push rbp
      58	  mov  rbp, rsp
      59	  mov rdi, rsp
      60	  call rukaml_initialize
      61		; expected_arity = 1
      62		; formal_arity = 1
      63		; calling "sq"
      64	  ; expected_arity = formal_arity = 1
      65	  sub rsp, 8 ; allocate for argument 0 (name = __temp11)
      66	  mov qword [rsp],  7
      67	  call sq
      68	  add rsp, 8 ; deallocate var "__temp11"
      69	  mov rax, rax
      70	  pop rbp
      71	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  ld: program.o: in function `main':
  program.asm:(.text+0x8a): undefined reference to `rukaml_initialize'
  [1]
