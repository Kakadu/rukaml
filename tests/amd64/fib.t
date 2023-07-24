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
      14	GLOBAL fib
      15	
      16	fib:
      17	  push rbp
      18	  mov  rbp, rsp
      19	  sub rsp, 8 ; allocate for var "temp1"
      20	  sub rsp, 8 ; allocate for var "__temp3"
      21	  mov rdx, [rsp+4*8] 
      22	  mov [rsp], rdx ; access a var "n"
      23	  sub rsp, 8 ; allocate for var "__temp4"
      24	  mov qword [rsp],  0
      25	  mov rax, [8*1+rsp]
      26	  mov rbx, [rsp]
      27	  cmp rax, rbx
      28	  je lab_11
      29	  mov qword [8*2+rsp], 0
      30	  jmp lab_12
      31	  lab_11:
      32	    mov qword [8*2+rsp], 1
      33	    jmp lab_12
      34	  lab_12:
      35	  add rsp, 8 ; deallocate var "__temp4"
      36	  add rsp, 8 ; deallocate var "__temp3"
      37	  mov rdx, [rsp+0*8] 
      38	  cmp rdx, 0
      39	  je lab_then_13
      40	  mov qword rax,  0
      41	  jmp lab_endif_14
      42	  lab_then_13:
      43	  sub rsp, 8 ; allocate for var "temp3"
      44	  sub rsp, 8 ; allocate for var "__temp5"
      45	  mov rdx, [rsp+5*8] 
      46	  mov [rsp], rdx ; access a var "n"
      47	  sub rsp, 8 ; allocate for var "__temp6"
      48	  mov qword [rsp],  1
      49	  mov rax, [8*1+rsp]
      50	  mov rbx, [rsp]
      51	  cmp rax, rbx
      52	  je lab_15
      53	  mov qword [8*2+rsp], 0
      54	  jmp lab_16
      55	  lab_15:
      56	    mov qword [8*2+rsp], 1
      57	    jmp lab_16
      58	  lab_16:
      59	  add rsp, 8 ; deallocate var "__temp6"
      60	  add rsp, 8 ; deallocate var "__temp5"
      61	  mov rdx, [rsp+0*8] 
      62	  cmp rdx, 0
      63	  je lab_then_17
      64	  mov qword rax,  1
      65	  jmp lab_endif_18
      66	  lab_then_17:
      67	  sub rsp, 8 ; allocate for var "temp5"
      68	  sub rsp, 8 ; allocate for var "__temp7"
      69	  mov rdx, [rsp+6*8] 
      70	  mov [rsp], rdx ; access a var "n"
      71	  mov rax, [rsp]
      72	  mov qword rbx, 2
      73	  sub rax, rbx
      74	  mov [8*1+rsp], rax
      75	  add rsp, 8 ; deallocate var "__temp7"
      76	  sub rsp, 8 ; allocate for var "temp6"
      77		; expected_arity = 1
      78		; formal_arity = 1
      79		; calling "fib"
      80	  ; expected_arity = formal_arity = 1
      81	  sub rsp, 8 ; allocate for argument 0 (name = __temp8)
      82	  mov rdx, [rsp+2*8] 
      83	  mov [rsp], rdx ; access a var "temp5"
      84	  call fib
      85	  add rsp, 8 ; deallocate var "__temp8"
      86	  mov [rsp], rax
      87	  sub rsp, 8 ; allocate for var "temp7"
      88	  sub rsp, 8 ; allocate for var "__temp9"
      89	  mov rdx, [rsp+8*8] 
      90	  mov [rsp], rdx ; access a var "n"
      91	  mov rax, [rsp]
      92	  dec rax
      93	  mov [8*1+rsp], rax
      94	  add rsp, 8 ; deallocate var "__temp9"
      95	  sub rsp, 8 ; allocate for var "temp8"
      96		; expected_arity = 1
      97		; formal_arity = 1
      98		; calling "fib"
      99	  ; expected_arity = formal_arity = 1
     100	  sub rsp, 8 ; allocate for argument 0 (name = __temp10)
     101	  mov rdx, [rsp+2*8] 
     102	  mov [rsp], rdx ; access a var "temp7"
     103	  call fib
     104	  add rsp, 8 ; deallocate var "__temp10"
     105	  mov [rsp], rax
     106	  sub rsp, 8 ; allocate for var "__temp11"
     107	  mov rdx, [rsp+3*8] 
     108	  mov [rsp], rdx ; access a var "temp6"
     109	  sub rsp, 8 ; allocate for var "__temp12"
     110	  mov rdx, [rsp+2*8] 
     111	  mov [rsp], rdx ; access a var "temp8"
     112	  mov rax, [8*1+rsp]
     113	  mov rbx, [rsp]
     114	  add  rbx, rax
     115	  mov rax, rbx
     116	  add rsp, 8 ; deallocate var "__temp12"
     117	  add rsp, 8 ; deallocate var "__temp11"
     118	  add rsp, 8 ; deallocate var "temp8"
     119	  add rsp, 8 ; deallocate var "temp7"
     120	  add rsp, 8 ; deallocate var "temp6"
     121	  add rsp, 8 ; deallocate var "temp5"
     122	  lab_endif_18:
     123	  add rsp, 8 ; deallocate var "temp3"
     124	  lab_endif_14:
     125	  add rsp, 8 ; deallocate var "temp1"
     126	  pop rbp
     127	  ret  ;;;; fib
     128	
     129		; @[{stack||stack}@]
     130	GLOBAL main
     131	main:
     132	  push rbp
     133	  mov  rbp, rsp
     134		; expected_arity = 1
     135		; formal_arity = 1
     136		; calling "fib"
     137	  ; expected_arity = formal_arity = 1
     138	  sub rsp, 8 ; allocate for argument 0 (name = __temp15)
     139	  mov qword [rsp],  8
     140	  call fib
     141	  add rsp, 8 ; deallocate var "__temp15"
     142	  mov rax, rax
     143	  pop rbp
     144	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  [21]
