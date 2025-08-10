  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  > let sq y = y * y
  > let double x = x+x
  > let main = sq 7
  > EOF
  ANF: let sq y =
         (y * y)
       let double x =
         (x + x)
       let main =
         sq 7 
  Location argument "y" in [rbp+2]
  Removing info about args [ y ]
  Location argument "x" in [rbp+2]
  Removing info about args [ x ]
  Removing info about args [  ]

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
      10	extern rukaml_print_alloc_closure_count
      11	
      12	_start:
      13	              push    rbp
      14	              mov     rbp, rsp   ; prologue
      15	              call main
      16	              mov rdi, rax    ; rdi stores return code
      17	              mov rax, 60     ; exit syscall
      18	              syscall
      19	GLOBAL sq
      20	
      21	sq:
      22	  push rbp
      23	  mov  rbp, rsp
      24	  mov qword r11, [rbp+2*8]
      25	  mov qword r12, [rbp+2*8]
      26	  imul r11, r12
      27	  mov rax, r11
      28	  pop rbp
      29	  ret  ;;;; sq
      30	GLOBAL double
      31	double:
      32	  push rbp
      33	  mov  rbp, rsp
      34	  mov qword r11, [rbp+2*8]
      35	  mov qword r12, [rbp+2*8]
      36	  add  r11, r12
      37	  mov rax, r11
      38	  pop rbp
      39	  ret  ;;;; double
      40	GLOBAL main
      41	main:
      42	  push rbp
      43	  mov  rbp, rsp
      44	  mov rdi, rsp
      45	  call rukaml_initialize
      46	  sub rsp, 8 ; trying to save alignment 16 bytes
      47	  sub rsp, 8*1 ; fun arguments
      48	  mov qword [rsp+0*8], 7 ; constant
      49	  call sq
      50	  add rsp, 8*2 ; dealloc args
      51	  mov rax, rax
      52	  pop rbp
      53	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe
  $ ./program.exe && echo $?
  [49]
