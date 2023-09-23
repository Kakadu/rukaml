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
      18	
      19		; @[{stack||stack}@]
      20	GLOBAL sq
      21	
      22	sq:
      23	  push rbp
      24	  mov  rbp, rsp
      25	  sub rsp, 8 ; allocate for var "__temp3"
      26	  mov rdx, [rsp+3*8] 
      27	  mov [rsp], rdx ; access a var "y"
      28	  sub rsp, 8 ; allocate for var "__temp4"
      29	  mov rdx, [rsp+4*8] 
      30	  mov [rsp], rdx ; access a var "y"
      31	  mov rax, [8*1+rsp]
      32	  mov rbx, [rsp]
      33	  imul rbx, rax
      34	  mov rax, rbx
      35	  add rsp, 8 ; deallocate var "__temp4"
      36	  add rsp, 8 ; deallocate var "__temp3"
      37	  pop rbp
      38	  ret  ;;;; sq
      39	
      40		; @[{stack||stack}@]
      41	GLOBAL double
      42	double:
      43	  push rbp
      44	  mov  rbp, rsp
      45	  sub rsp, 8 ; allocate for var "__temp7"
      46	  mov rdx, [rsp+3*8] 
      47	  mov [rsp], rdx ; access a var "x"
      48	  sub rsp, 8 ; allocate for var "__temp8"
      49	  mov rdx, [rsp+4*8] 
      50	  mov [rsp], rdx ; access a var "x"
      51	  mov rax, [8*1+rsp]
      52	  mov rbx, [rsp]
      53	  add  rbx, rax
      54	  mov rax, rbx
      55	  add rsp, 8 ; deallocate var "__temp8"
      56	  add rsp, 8 ; deallocate var "__temp7"
      57	  pop rbp
      58	  ret  ;;;; double
      59	
      60		; @[{stack||stack}@]
      61	GLOBAL main
      62	main:
      63	  push rbp
      64	  mov  rbp, rsp
      65	mov rdi, rsp
      66	call rukaml_initialize
      67		; expected_arity = 1
      68		; formal_arity = 1
      69		; calling "sq"
      70	  ; expected_arity = formal_arity = 1
      71	  sub rsp, 8 ; allocate for argument 0 (name = __temp11)
      72	  mov qword [rsp],  7
      73	  call sq
      74	  add rsp, 8 ; deallocate var "__temp11"
      75	  mov rax, rax
      76	  pop rbp
      77	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  ld: program.o: in function `main':
  program.asm:(.text+0x8a): undefined reference to `rukaml_initialize'
  [1]
