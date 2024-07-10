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
      23	  mov qword r11, [rbp+2*8]
      24	  mov qword r12, [rbp+2*8]
      25	  imul r11, r12
      26	  mov rax, r11
      27	  pop rbp
      28	  ret  ;;;; sq
      29	GLOBAL double
      30	double:
      31	  push rbp
      32	  mov  rbp, rsp
      33	  mov qword r11, [rbp+2*8]
      34	  mov qword r12, [rbp+2*8]
      35	  add  r11, r12
      36	  mov rax, r11
      37	  pop rbp
      38	  ret  ;;;; double
      39	GLOBAL main
      40	main:
      41	  push rbp
      42	  mov  rbp, rsp
      43	  mov rdi, rsp
      44	  call rukaml_initialize
      45	  sub rsp, 8 ; trying to save alignment 16 bytes
      46	  sub rsp, 8*1 ; fun arguments
      47	  mov qword [rsp+0*8], 7 ; constant
      48	  call sq
      49	  add rsp, 8*2 ; dealloc args
      50	  mov rax, rax
      51	  pop rbp
      52	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe
  $ ./program.exe && echo $?
  [49]
