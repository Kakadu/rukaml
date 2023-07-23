  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  > let rec fib n =
  >   if n=0 then 0 else if n=1 then 1 else fib (n-2) + fib (n-1)
  > let main = fib 8
  > EOF
  After ANF transformation.
  let rec fib n =
    let temp1 = (n = 0) in
      (if temp1
      then 0
      else let temp3 = (n = 1) in
             (if temp3
             then 1
             else let temp5 = (n - 2) in
                    let temp6 = fib temp5  in
                      let temp7 = (n - 1) in
                        let temp8 = fib temp7  in
                          (temp6 + temp8)))
  let main =
    fib 8 
  ANF: let rec fib n =
         let temp1 = (n = 0) in
           (if temp1
           then 0
           else let temp3 = (n = 1) in
                  (if temp3
                  then 1
                  else let temp5 = (n - 2) in
                         let temp6 = fib temp5  in
                           let temp7 = (n - 1) in
                             let temp8 = fib temp7  in
                               (temp6 + temp8)))
       let main =
         fib 8 

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
      47	fib:
      48	  push rbp
      49	  mov  rbp, rsp
      50	  sub rsp, 8 ; allocate for var "temp1"
      51	  sub rsp, 8 ; allocate for var "__temp3"
      52	  mov rdx, [rsp+4*8] 
      53	  mov [8*0+rsp], rdx ; access a var "n"
      54	  sub rsp, 8 ; allocate for var "__temp4"
      55	  mov qword [8*0+rsp],  0
      56	  mov rax, [8*1+rsp]
      57	  mov rbx, [8*0+rsp]
      58	  cmp rax, rbx
      59	  je lab_11
      60	  mov qword [8*2+rsp], 0
      61	  jmp lab_12
      62	  lab_11:
      63	    mov qword [8*2+rsp], 1
      64	    jmp lab_12
      65	  lab_12:
      66	  add rsp, 8 ; deallocate var "__temp4"
      67	  add rsp, 8 ; deallocate var "__temp3"
      68	  mov rdx, [rsp+0*8] 
      69	  cmp rdx, 0
      70	  je lab_then_13
      71	  mov qword rax,  0
      72	  jmp lab_endif_14
      73	  lab_then_13:
      74	  sub rsp, 8 ; allocate for var "temp3"
      75	  sub rsp, 8 ; allocate for var "__temp5"
      76	  mov rdx, [rsp+5*8] 
      77	  mov [8*0+rsp], rdx ; access a var "n"
      78	  sub rsp, 8 ; allocate for var "__temp6"
      79	  mov qword [8*0+rsp],  1
      80	  mov rax, [8*1+rsp]
      81	  mov rbx, [8*0+rsp]
      82	  cmp rax, rbx
      83	  je lab_15
      84	  mov qword [8*2+rsp], 0
      85	  jmp lab_16
      86	  lab_15:
      87	    mov qword [8*2+rsp], 1
      88	    jmp lab_16
      89	  lab_16:
      90	  add rsp, 8 ; deallocate var "__temp6"
      91	  add rsp, 8 ; deallocate var "__temp5"
      92	  mov rdx, [rsp+0*8] 
      93	  cmp rdx, 0
      94	  je lab_then_17
      95	  mov qword rax,  1
      96	  jmp lab_endif_18
      97	  lab_then_17:
      98	  sub rsp, 8 ; allocate for var "temp5"
      99	  sub rsp, 8 ; allocate for var "__temp7"
     100	  mov rdx, [rsp+6*8] 
     101	  mov [8*0+rsp], rdx ; access a var "n"
     102	  mov rax, [8*0+rsp]
     103	  mov qword rbx, 2
     104	  sub rax, rbx
     105	  mov [8*1+rsp], rax
     106	  add rsp, 8 ; deallocate var "__temp7"
     107	  sub rsp, 8 ; allocate for var "temp6"
     108	  sub rsp, 8 ; allocate for var "__temp8"
     109	  mov rdx, [rsp+2*8] 
     110	  mov [8*0+rsp], rdx ; access a var "temp5"
     111	  call fib
     112	  add rsp, 8 ; deallocate var "__temp8"
     113	  mov [8*0+rsp], rax
     114	  sub rsp, 8 ; allocate for var "temp7"
     115	  sub rsp, 8 ; allocate for var "__temp9"
     116	  mov rdx, [rsp+8*8] 
     117	  mov [8*0+rsp], rdx ; access a var "n"
     118	  mov rax, [8*0+rsp]
     119	  dec rax
     120	  mov [8*1+rsp], rax
     121	  add rsp, 8 ; deallocate var "__temp9"
     122	  sub rsp, 8 ; allocate for var "temp8"
     123	  sub rsp, 8 ; allocate for var "__temp10"
     124	  mov rdx, [rsp+2*8] 
     125	  mov [8*0+rsp], rdx ; access a var "temp7"
     126	  call fib
     127	  add rsp, 8 ; deallocate var "__temp10"
     128	  mov [8*0+rsp], rax
     129	  sub rsp, 8 ; allocate for var "__temp11"
     130	  mov rdx, [rsp+3*8] 
     131	  mov [8*0+rsp], rdx ; access a var "temp6"
     132	  sub rsp, 8 ; allocate for var "__temp12"
     133	  mov rdx, [rsp+2*8] 
     134	  mov [8*0+rsp], rdx ; access a var "temp8"
     135	  mov rax, [8*1+rsp]
     136	  mov rbx, [8*0+rsp]
     137	  add  rbx, rax
     138	  mov rax, rbx
     139	  add rsp, 8 ; deallocate var "__temp12"
     140	  add rsp, 8 ; deallocate var "__temp11"
     141	  add rsp, 8 ; deallocate var "temp8"
     142	  add rsp, 8 ; deallocate var "temp7"
     143	  add rsp, 8 ; deallocate var "temp6"
     144	  add rsp, 8 ; deallocate var "temp5"
     145	  lab_endif_18:
     146	  add rsp, 8 ; deallocate var "temp3"
     147	  lab_endif_14:
     148	  add rsp, 8 ; deallocate var "temp1"
     149	  pop rbp
     150	  ret  ;;;; fib
     151	
     152		; @[{stack||stack}@]
     153	main:
     154	  push rbp
     155	  mov  rbp, rsp
     156	  sub rsp, 8 ; allocate for var "__temp15"
     157	  mov qword [8*0+rsp],  8
     158	  call fib
     159	  add rsp, 8 ; deallocate var "__temp15"
     160	  mov rax, rax
     161	  pop rbp
     162	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  [21]
