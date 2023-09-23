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
      20	GLOBAL fib
      21	
      22	fib:
      23	  push rbp
      24	  mov  rbp, rsp
      25	  sub rsp, 8 ; allocate for var "temp1"
      26	  sub rsp, 8 ; allocate for var "__temp3"
      27	  mov rdx, [rsp+4*8] 
      28	  mov [rsp], rdx ; access a var "n"
      29	  sub rsp, 8 ; allocate for var "__temp4"
      30	  mov qword [rsp],  0
      31	  mov rax, [8*1+rsp]
      32	  mov rbx, [rsp]
      33	  cmp rax, rbx
      34	  je lab_11
      35	  mov qword [8*2+rsp], 0
      36	  jmp lab_12
      37	  lab_11:
      38	    mov qword [8*2+rsp], 1
      39	    jmp lab_12
      40	  lab_12:
      41	  add rsp, 8 ; deallocate var "__temp4"
      42	  add rsp, 8 ; deallocate var "__temp3"
      43	  mov rdx, [rsp+0*8] 
      44	  cmp rdx, 0
      45	  je lab_then_13
      46	  mov qword rax,  0
      47	  jmp lab_endif_14
      48	  lab_then_13:
      49	  sub rsp, 8 ; allocate for var "temp3"
      50	  sub rsp, 8 ; allocate for var "__temp5"
      51	  mov rdx, [rsp+5*8] 
      52	  mov [rsp], rdx ; access a var "n"
      53	  sub rsp, 8 ; allocate for var "__temp6"
      54	  mov qword [rsp],  1
      55	  mov rax, [8*1+rsp]
      56	  mov rbx, [rsp]
      57	  cmp rax, rbx
      58	  je lab_15
      59	  mov qword [8*2+rsp], 0
      60	  jmp lab_16
      61	  lab_15:
      62	    mov qword [8*2+rsp], 1
      63	    jmp lab_16
      64	  lab_16:
      65	  add rsp, 8 ; deallocate var "__temp6"
      66	  add rsp, 8 ; deallocate var "__temp5"
      67	  mov rdx, [rsp+0*8] 
      68	  cmp rdx, 0
      69	  je lab_then_17
      70	  mov qword rax,  1
      71	  jmp lab_endif_18
      72	  lab_then_17:
      73	  sub rsp, 8 ; allocate for var "temp5"
      74	  sub rsp, 8 ; allocate for var "__temp7"
      75	  mov rdx, [rsp+6*8] 
      76	  mov [rsp], rdx ; access a var "n"
      77	  mov rax, [rsp]
      78	  mov qword rbx, 2
      79	  sub rax, rbx
      80	  mov [8*1+rsp], rax
      81	  add rsp, 8 ; deallocate var "__temp7"
      82	  sub rsp, 8 ; allocate for var "temp6"
      83		; expected_arity = 1
      84		; formal_arity = 1
      85		; calling "fib"
      86	  ; expected_arity = formal_arity = 1
      87	  sub rsp, 8 ; allocate for argument 0 (name = __temp8)
      88	  mov rdx, [rsp+2*8] 
      89	  mov [rsp], rdx ; access a var "temp5"
      90	  call fib
      91	  add rsp, 8 ; deallocate var "__temp8"
      92	  mov [rsp], rax
      93	  sub rsp, 8 ; allocate for var "temp7"
      94	  sub rsp, 8 ; allocate for var "__temp9"
      95	  mov rdx, [rsp+8*8] 
      96	  mov [rsp], rdx ; access a var "n"
      97	  mov rax, [rsp]
      98	  dec rax
      99	  mov [8*1+rsp], rax
     100	  add rsp, 8 ; deallocate var "__temp9"
     101	  sub rsp, 8 ; allocate for var "temp8"
     102		; expected_arity = 1
     103		; formal_arity = 1
     104		; calling "fib"
     105	  ; expected_arity = formal_arity = 1
     106	  sub rsp, 8 ; allocate for argument 0 (name = __temp10)
     107	  mov rdx, [rsp+2*8] 
     108	  mov [rsp], rdx ; access a var "temp7"
     109	  call fib
     110	  add rsp, 8 ; deallocate var "__temp10"
     111	  mov [rsp], rax
     112	  sub rsp, 8 ; allocate for var "__temp11"
     113	  mov rdx, [rsp+3*8] 
     114	  mov [rsp], rdx ; access a var "temp6"
     115	  sub rsp, 8 ; allocate for var "__temp12"
     116	  mov rdx, [rsp+2*8] 
     117	  mov [rsp], rdx ; access a var "temp8"
     118	  mov rax, [8*1+rsp]
     119	  mov rbx, [rsp]
     120	  add  rbx, rax
     121	  mov rax, rbx
     122	  add rsp, 8 ; deallocate var "__temp12"
     123	  add rsp, 8 ; deallocate var "__temp11"
     124	  add rsp, 8 ; deallocate var "temp8"
     125	  add rsp, 8 ; deallocate var "temp7"
     126	  add rsp, 8 ; deallocate var "temp6"
     127	  add rsp, 8 ; deallocate var "temp5"
     128	  lab_endif_18:
     129	  add rsp, 8 ; deallocate var "temp3"
     130	  lab_endif_14:
     131	  add rsp, 8 ; deallocate var "temp1"
     132	  pop rbp
     133	  ret  ;;;; fib
     134	
     135		; @[{stack||stack}@]
     136	GLOBAL main
     137	main:
     138	  push rbp
     139	  mov  rbp, rsp
     140	mov rdi, rsp
     141	call rukaml_initialize
     142		; expected_arity = 1
     143		; formal_arity = 1
     144		; calling "fib"
     145	  ; expected_arity = formal_arity = 1
     146	  sub rsp, 8 ; allocate for argument 0 (name = __temp15)
     147	  mov qword [rsp],  8
     148	  call fib
     149	  add rsp, 8 ; deallocate var "__temp15"
     150	  mov rax, rax
     151	  pop rbp
     152	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  ld: program.o: in function `main':
  program.asm:(.text+0x1a7): undefined reference to `rukaml_initialize'
  [1]
