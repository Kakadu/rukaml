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
      15	GLOBAL fib
      16	
      17	fib:
      18	  push rbp
      19	  mov  rbp, rsp
      20	  sub rsp, 8 ; allocate for var "temp1"
      21	  sub rsp, 8 ; allocate for var "__temp3"
      22	  mov rdx, [rsp+4*8] 
      23	  mov [rsp], rdx ; access a var "n"
      24	  sub rsp, 8 ; allocate for var "__temp4"
      25	  mov qword [rsp],  0
      26	  mov rax, [8*1+rsp]
      27	  mov rbx, [rsp]
      28	  cmp rax, rbx
      29	  je lab_11
      30	  mov qword [8*2+rsp], 0
      31	  jmp lab_12
      32	  lab_11:
      33	    mov qword [8*2+rsp], 1
      34	    jmp lab_12
      35	  lab_12:
      36	  add rsp, 8 ; deallocate var "__temp4"
      37	  add rsp, 8 ; deallocate var "__temp3"
      38	  mov rdx, [rsp+0*8] 
      39	  cmp rdx, 0
      40	  je lab_then_13
      41	  mov qword rax,  0
      42	  jmp lab_endif_14
      43	  lab_then_13:
      44	  sub rsp, 8 ; allocate for var "temp3"
      45	  sub rsp, 8 ; allocate for var "__temp5"
      46	  mov rdx, [rsp+5*8] 
      47	  mov [rsp], rdx ; access a var "n"
      48	  sub rsp, 8 ; allocate for var "__temp6"
      49	  mov qword [rsp],  1
      50	  mov rax, [8*1+rsp]
      51	  mov rbx, [rsp]
      52	  cmp rax, rbx
      53	  je lab_15
      54	  mov qword [8*2+rsp], 0
      55	  jmp lab_16
      56	  lab_15:
      57	    mov qword [8*2+rsp], 1
      58	    jmp lab_16
      59	  lab_16:
      60	  add rsp, 8 ; deallocate var "__temp6"
      61	  add rsp, 8 ; deallocate var "__temp5"
      62	  mov rdx, [rsp+0*8] 
      63	  cmp rdx, 0
      64	  je lab_then_17
      65	  mov qword rax,  1
      66	  jmp lab_endif_18
      67	  lab_then_17:
      68	  sub rsp, 8 ; allocate for var "temp5"
      69	  sub rsp, 8 ; allocate for var "__temp7"
      70	  mov rdx, [rsp+6*8] 
      71	  mov [rsp], rdx ; access a var "n"
      72	  mov rax, [rsp]
      73	  mov qword rbx, 2
      74	  sub rax, rbx
      75	  mov [8*1+rsp], rax
      76	  add rsp, 8 ; deallocate var "__temp7"
      77	  sub rsp, 8 ; allocate for var "temp6"
      78		; expected_arity = 1
      79		; formal_arity = 1
      80		; calling "fib"
      81	  ; expected_arity = formal_arity = 1
      82	  sub rsp, 8 ; allocate for argument 0 (name = __temp8)
      83	  mov rdx, [rsp+2*8] 
      84	  mov [rsp], rdx ; access a var "temp5"
      85	  call fib
      86	  add rsp, 8 ; deallocate var "__temp8"
      87	  mov [rsp], rax
      88	  sub rsp, 8 ; allocate for var "temp7"
      89	  sub rsp, 8 ; allocate for var "__temp9"
      90	  mov rdx, [rsp+8*8] 
      91	  mov [rsp], rdx ; access a var "n"
      92	  mov rax, [rsp]
      93	  dec rax
      94	  mov [8*1+rsp], rax
      95	  add rsp, 8 ; deallocate var "__temp9"
      96	  sub rsp, 8 ; allocate for var "temp8"
      97		; expected_arity = 1
      98		; formal_arity = 1
      99		; calling "fib"
     100	  ; expected_arity = formal_arity = 1
     101	  sub rsp, 8 ; allocate for argument 0 (name = __temp10)
     102	  mov rdx, [rsp+2*8] 
     103	  mov [rsp], rdx ; access a var "temp7"
     104	  call fib
     105	  add rsp, 8 ; deallocate var "__temp10"
     106	  mov [rsp], rax
     107	  sub rsp, 8 ; allocate for var "__temp11"
     108	  mov rdx, [rsp+3*8] 
     109	  mov [rsp], rdx ; access a var "temp6"
     110	  sub rsp, 8 ; allocate for var "__temp12"
     111	  mov rdx, [rsp+2*8] 
     112	  mov [rsp], rdx ; access a var "temp8"
     113	  mov rax, [8*1+rsp]
     114	  mov rbx, [rsp]
     115	  add  rbx, rax
     116	  mov rax, rbx
     117	  add rsp, 8 ; deallocate var "__temp12"
     118	  add rsp, 8 ; deallocate var "__temp11"
     119	  add rsp, 8 ; deallocate var "temp8"
     120	  add rsp, 8 ; deallocate var "temp7"
     121	  add rsp, 8 ; deallocate var "temp6"
     122	  add rsp, 8 ; deallocate var "temp5"
     123	  lab_endif_18:
     124	  add rsp, 8 ; deallocate var "temp3"
     125	  lab_endif_14:
     126	  add rsp, 8 ; deallocate var "temp1"
     127	  pop rbp
     128	  ret  ;;;; fib
     129	
     130		; @[{stack||stack}@]
     131	GLOBAL main
     132	main:
     133	  push rbp
     134	  mov  rbp, rsp
     135		; expected_arity = 1
     136		; formal_arity = 1
     137		; calling "fib"
     138	  ; expected_arity = formal_arity = 1
     139	  sub rsp, 8 ; allocate for argument 0 (name = __temp15)
     140	  mov qword [rsp],  8
     141	  call fib
     142	  add rsp, 8 ; deallocate var "__temp15"
     143	  mov rax, rax
     144	  pop rbp
     145	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  [21]
