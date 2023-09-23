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
      18	
      19		; @[{stack||stack}@]
      20	GLOBAL main
      21	
      22	main:
      23	  push rbp
      24	  mov  rbp, rsp
      25	mov rdi, rsp
      26	call rukaml_initialize
      27	  sub rsp, 8 ; allocate for var "temp1"
      28	  sub rsp, 8 ; allocate for var "__temp3"
      29	  mov qword [rsp],  0
      30	  sub rsp, 8 ; allocate for var "__temp4"
      31	  mov qword [rsp],  1
      32	  mov rax, [8*1+rsp]
      33	  mov rbx, [rsp]
      34	  cmp rax, rbx
      35	  je lab_3
      36	  mov qword [8*2+rsp], 0
      37	  jmp lab_4
      38	  lab_3:
      39	    mov qword [8*2+rsp], 1
      40	    jmp lab_4
      41	  lab_4:
      42	  add rsp, 8 ; deallocate var "__temp4"
      43	  add rsp, 8 ; deallocate var "__temp3"
      44	  mov rdx, [rsp+0*8] 
      45	  cmp rdx, 0
      46	  je lab_then_5
      47	  mov qword rax,  10
      48	  jmp lab_endif_6
      49	  lab_then_5:
      50	  mov qword rax,  20
      51	  lab_endif_6:
      52	  add rsp, 8 ; deallocate var "temp1"
      53	  pop rbp
      54	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  ld: program.o: in function `main':
  program.asm:(.text+0x1b): undefined reference to `rukaml_initialize'
  [1]
