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
       3	extern rukaml_applyN
       4	
       5	_start:
       6	              push    rbp
       7	              mov     rbp, rsp   ; prologue
       8	              call main
       9	              mov rdi, rax    ; rdi stores return code
      10	              mov rax, 60     ; exit syscall
      11	              syscall
      12	
      13		; @[{stack||stack}@]
      14	GLOBAL main
      15	
      16	main:
      17	  push rbp
      18	  mov  rbp, rsp
      19	  sub rsp, 8 ; allocate for var "temp1"
      20	  sub rsp, 8 ; allocate for var "__temp3"
      21	  mov qword [rsp],  0
      22	  sub rsp, 8 ; allocate for var "__temp4"
      23	  mov qword [rsp],  1
      24	  mov rax, [8*1+rsp]
      25	  mov rbx, [rsp]
      26	  cmp rax, rbx
      27	  je lab_3
      28	  mov qword [8*2+rsp], 0
      29	  jmp lab_4
      30	  lab_3:
      31	    mov qword [8*2+rsp], 1
      32	    jmp lab_4
      33	  lab_4:
      34	  add rsp, 8 ; deallocate var "__temp4"
      35	  add rsp, 8 ; deallocate var "__temp3"
      36	  mov rdx, [rsp+0*8] 
      37	  cmp rdx, 0
      38	  je lab_then_5
      39	  mov qword rax,  10
      40	  jmp lab_endif_6
      41	  lab_then_5:
      42	  mov qword rax,  20
      43	  lab_endif_6:
      44	  add rsp, 8 ; deallocate var "temp1"
      45	  pop rbp
      46	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  [20]
