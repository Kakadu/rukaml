  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm - #-vamd64
  > let main = print (if 1=1 then 10 else 20)
  > EOF
  After ANF transformation.
  let main =
    let temp1 = (1 = 1) in
      let temp2 = (if temp1
                  then 10
                  else 20) in
        print temp2

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
      25	  sub rsp, 8*2 ; allocate for local variables temp2, temp1
      26	  mov qword [rbp-1*8], 1
      27	  mov qword rdx, [rbp-1*8]
      28	  cmp rdx, 0
      29	  je lab_then_4
      30	  mov qword [rbp-2*8],  10
      31	  jmp lab_endif_5
      32	lab_then_4:
      33	  mov qword [rbp-2*8],  20
      34	lab_endif_5:
      35	  mov rdi, [rbp-2*8]
      36	  call rukaml_print_int ; short
      37	  mov rax, rax
      38	  add rsp, 8*2 ; deallocate local variables temp2, temp1
      39	  pop rbp
      40	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe
  $ ./program.exe && echo $?
  rukaml_print_int 10
  0
