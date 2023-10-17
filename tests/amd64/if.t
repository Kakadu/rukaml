  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  > let main = if 1=1 then 10 else 20
  > EOF
  After ANF transformation.
  let main =
    let temp1 = (1 = 1) in
      (if temp1
      then 10
      else 20)
  ANF: let main =
         let temp1 = (1 = 1) in
           (if temp1
           then 10
           else 20)
  extend temp1 with shift = 1
  extend __pad3 with shift = 2
  remove __pad3 with shift = 2
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
      18	GLOBAL main
      19	
      20	main:
      21	  push rbp
      22	  mov  rbp, rsp
      23	  mov rdi, rsp
      24	  call rukaml_initialize
      25	  sub rsp, 8*1 ; allocate for local variables temp1
      26	  sub rsp, 8 ; allocate padding for locals
      27	    ;; calculate rhs and put into [rbp-1*8]. offset = 1
      28	  mov qword [rbp-1*8], 1
      29	  mov qword rdx, [rbp-1*8]
      30	  cmp rdx, 0
      31	  je lab_then_4
      32	  mov qword [rbp-1*8],  10
      33	  jmp lab_endif_5
      34	lab_then_4:
      35	  mov qword [rbp-1*8],  20
      36	lab_endif_5:
      37	  add rsp, 8 ; deallocate padding for locals
      38	  add rsp, 8*1 ; deallocate local variables temp1
      39	  pop rbp
      40	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc-12 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe 
  $ ./program.exe && echo $?
  0
