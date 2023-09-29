  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  > let main = if 0=1 then 10 else 20
  > EOF
  After ANF transformation.
  let main =
    let temp1 = (0 = 1) in
      (if temp1
      then 10
      else 20)
  ANF: let main =
         let temp1 = (0 = 1) in
           (if temp1
           then 10
           else 20)

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
      18	GLOBAL main
      19	
      20	main:
      21	  push rbp
      22	  mov  rbp, rsp
      23	  mov rdi, rsp
      24	  call rukaml_initialize
      25	  sub rsp, 8 ; allocate for var "temp1"
      26	  sub rsp, 8 ; allocate for var "__temp3"
      27	  mov qword [rsp],  0
      28	  sub rsp, 8 ; allocate for var "__temp4"
      29	  mov qword [rsp],  1
      30	  mov rax, [8*1+rsp]
      31	  mov rbx, [rsp]
      32	  cmp rax, rbx
      33	  je lab_3
      34	  mov qword [8*2+rsp], 0
      35	  jmp lab_4
      36	lab_3:
      37	  mov qword [8*2+rsp], 1
      38	  jmp lab_4
      39	lab_4:
      40	  add rsp, 8 ; deallocate var "__temp4"
      41	  add rsp, 8 ; deallocate var "__temp3"
      42	  mov rdx, [rsp+0*8] 
      43	  cmp rdx, 0
      44	  je lab_then_5
      45	  mov qword rax,  10
      46	  jmp lab_endif_6
      47	  lab_then_5:
      48	  mov qword rax,  20
      49	  lab_endif_6:
      50	  add rsp, 8 ; deallocate var "temp1"
      51	  pop rbp
      52	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  ld: program.o: in function `main':
  program.asm:(.text+0x1b): undefined reference to `rukaml_initialize'
  [1]
