  $ cat << EOF | ../../back/amd64/amd64_compiler.exe -o program.asm - #-vamd64
  > let main = print (if 1=1 then 10 else 20)
  > EOF

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
      19	GLOBAL main
      20	
      21	main:
      22	  push rbp
      23	  mov  rbp, rsp
      24	  mov rdi, rsp
      25	  call rukaml_initialize
      26	  sub rsp, 8*2 ; allocate for local variables temp2, temp1
      27	  mov qword [rbp-1*8], 1
      28	  mov qword rdx, [rbp-1*8]
      29	  cmp rdx, 0
      30	  je lab_then_4
      31	  mov qword [rbp-2*8],  10
      32	  jmp lab_endif_5
      33	lab_then_4:
      34	  mov qword [rbp-2*8],  20
      35	lab_endif_5:
      36	  add rsp, -8*2
      37	  mov r11, [rbp-2*8]
      38	  mov qword [rsp], r11
      39	  call rukaml_print_int ; short
      40	  add rsp, 8*2
      41	  mov rax, rax
      42	  add rsp, 8*2 ; deallocate local variables temp2, temp1
      43	  pop rbp
      44	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 program.o ../../back/amd64/rukaml_stdlib.o -o program.exe
  $ ./program.exe && echo $?
  rukaml_print_int 10
  0
