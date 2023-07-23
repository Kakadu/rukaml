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
       1	section .data
       2	            newline_char: db 10
       3	            codes: db '0123456789abcdef' 
       4	section .text
       5	global _start
       6	print_newline:
       7	          mov rax, 1 ; 'write' syscall identifier
       8	          mov rdi, 1 ; stdout file descriptor
       9	          mov rsi, newline_char ; where do we take data from
      10	          mov rdx, 1 ; the amount of bytes to write
      11	          syscall
      12	          ret 
      13	
      14	print_hex:
      15	  mov rax, rdi
      16	  mov rdi, 1
      17	  mov rdx, 1
      18	  mov rcx, 64 ; how far are we shifting rax?
      19	iterate:
      20	  push rax ; Save the initial rax value
      21	  sub rcx, 4
      22	  sar rax, cl ; shift to 60, 56, 52, ... 4, 0
      23	              ; the cl register is the smallest part of rcx
      24	  and rax, 0xf ; clear all bits but the lowest four
      25	  lea rsi, [codes + rax]; take a hexadecimal digit character code
      26	  mov rax, 1
      27	  push rcx  ; syscall will break rcx
      28	  syscall   ; rax = 1 (31) -- the write identifier,
      29	            ; rdi = 1 for stdout,
      30	            ; rsi = the address of a character, see line 29
      31	  pop rcx
      32	  pop rax          ; ˆ see line 24 ˆ
      33	  test rcx, rcx    ; rcx = 0 when all digits are shown
      34	  jnz iterate
      35	  ret
      36	
      37	_start:
      38	              push    rbp
      39	              mov     rbp, rsp   ; prologue
      40	              call main
      41	              mov rdi, rax    ; rdi stores return code
      42	              mov rax, 60     ; exit syscall
      43	              syscall
      44	
      45		; @[{stack||stack}@]
      46	
      47	main:
      48	  push rbp
      49	  mov  rbp, rsp
      50	  sub rsp, 8 ; allocate for var "temp1"
      51	  sub rsp, 8 ; allocate for var "__temp3"
      52	  mov qword [8*0+rsp],  0
      53	  sub rsp, 8 ; allocate for var "__temp4"
      54	  mov qword [8*0+rsp],  1
      55	  mov rax, [8*1+rsp]
      56	  mov rbx, [8*0+rsp]
      57	  cmp rax, rbx
      58	  je lab_3
      59	  mov qword [8*2+rsp], 0
      60	  jmp lab_4
      61	  lab_3:
      62	    mov qword [8*2+rsp], 1
      63	    jmp lab_4
      64	  lab_4:
      65	  add rsp, 8 ; deallocate var "__temp4"
      66	  add rsp, 8 ; deallocate var "__temp3"
      67	  mov rdx, [rsp+0*8] 
      68	  cmp rdx, 0
      69	  je lab_then_5
      70	  mov qword rax,  10
      71	  jmp lab_endif_6
      72	  lab_then_5:
      73	  mov qword rax,  20
      74	  lab_endif_6:
      75	  add rsp, 8 ; deallocate var "temp1"
      76	  pop rbp
      77	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  [20]
