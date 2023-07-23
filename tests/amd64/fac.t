  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  > let rec fac n =
  >   if n=1 then 1 else n * fac (n-1)
  > let main = fac 5
  > EOF
  After ANF transformation.
  let rec fac n =
    let temp1 = (n = 1) in
      (if temp1
      then 1
      else let temp3 = (n - 1) in
             let temp4 = fac temp3  in
               (n * temp4))
  let main =
    fac 5 
  ANF: let rec fac n =
         let temp1 = (n = 1) in
           (if temp1
           then 1
           else let temp3 = (n - 1) in
                  let temp4 = fac temp3  in
                    (n * temp4))
       let main =
         fac 5 

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
      47	fac:
      48	  push rbp
      49	  mov  rbp, rsp
      50	  sub rsp, 8 ; allocate for var "temp1"
      51	  sub rsp, 8 ; allocate for var "__temp3"
      52	  mov rdx, [rsp+4*8] 
      53	  mov [8*0+rsp], rdx ; access a var "n"
      54	  sub rsp, 8 ; allocate for var "__temp4"
      55	  mov qword [8*0+rsp],  1
      56	  mov rax, [8*1+rsp]
      57	  mov rbx, [8*0+rsp]
      58	  cmp rax, rbx
      59	  je lab_7
      60	  mov qword [8*2+rsp], 0
      61	  jmp lab_8
      62	  lab_7:
      63	    mov qword [8*2+rsp], 1
      64	    jmp lab_8
      65	  lab_8:
      66	  add rsp, 8 ; deallocate var "__temp4"
      67	  add rsp, 8 ; deallocate var "__temp3"
      68	  mov rdx, [rsp+0*8] 
      69	  cmp rdx, 0
      70	  je lab_then_9
      71	  mov qword rax,  1
      72	  jmp lab_endif_10
      73	  lab_then_9:
      74	  sub rsp, 8 ; allocate for var "temp3"
      75	  sub rsp, 8 ; allocate for var "__temp5"
      76	  mov rdx, [rsp+5*8] 
      77	  mov [8*0+rsp], rdx ; access a var "n"
      78	  mov rax, [8*0+rsp]
      79	  dec rax
      80	  mov [8*1+rsp], rax
      81	  add rsp, 8 ; deallocate var "__temp5"
      82	  sub rsp, 8 ; allocate for var "temp4"
      83	  sub rsp, 8 ; allocate for var "__temp6"
      84	  mov rdx, [rsp+2*8] 
      85	  mov [8*0+rsp], rdx ; access a var "temp3"
      86	  call fac
      87	  add rsp, 8 ; deallocate var "__temp6"
      88	  mov [8*0+rsp], rax
      89	  sub rsp, 8 ; allocate for var "__temp7"
      90	  mov rdx, [rsp+6*8] 
      91	  mov [8*0+rsp], rdx ; access a var "n"
      92	  sub rsp, 8 ; allocate for var "__temp8"
      93	  mov rdx, [rsp+2*8] 
      94	  mov [8*0+rsp], rdx ; access a var "temp4"
      95	  mov rax, [8*1+rsp]
      96	  mov rbx, [8*0+rsp]
      97	  imul rbx, rax
      98	  mov rax, rbx
      99	  add rsp, 8 ; deallocate var "__temp8"
     100	  add rsp, 8 ; deallocate var "__temp7"
     101	  add rsp, 8 ; deallocate var "temp4"
     102	  add rsp, 8 ; deallocate var "temp3"
     103	  lab_endif_10:
     104	  add rsp, 8 ; deallocate var "temp1"
     105	  pop rbp
     106	  ret  ;;;; fac
     107	
     108		; @[{stack||stack}@]
     109	main:
     110	  push rbp
     111	  mov  rbp, rsp
     112	  sub rsp, 8 ; allocate for var "__temp11"
     113	  mov qword [8*0+rsp],  5
     114	  call fac
     115	  add rsp, 8 ; deallocate var "__temp11"
     116	  mov rax, rax
     117	  pop rbp
     118	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  [120]
