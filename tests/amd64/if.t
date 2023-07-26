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
      15	GLOBAL main
      16	
      17	main:
      18	  push rbp
      19	  mov  rbp, rsp
      20	  sub rsp, 8 ; allocate for var "temp1"
      21	  sub rsp, 8 ; allocate for var "__temp3"
      22	  mov qword [rsp],  0
      23	  sub rsp, 8 ; allocate for var "__temp4"
      24	  mov qword [rsp],  1
      25	  mov rax, [8*1+rsp]
      26	  mov rbx, [rsp]
      27	  cmp rax, rbx
      28	  je lab_3
      29	  mov qword [8*2+rsp], 0
      30	  jmp lab_4
      31	  lab_3:
      32	    mov qword [8*2+rsp], 1
      33	    jmp lab_4
      34	  lab_4:
      35	  add rsp, 8 ; deallocate var "__temp4"
      36	  add rsp, 8 ; deallocate var "__temp3"
      37	  mov rdx, [rsp+0*8] 
      38	  cmp rdx, 0
      39	  je lab_then_5
      40	  mov qword rax,  10
      41	  jmp lab_endif_6
      42	  lab_then_5:
      43	  mov qword rax,  20
      44	  lab_endif_6:
      45	  add rsp, 8 ; deallocate var "temp1"
      46	  pop rbp
      47	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  [20]
