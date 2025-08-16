  $ cat << EOF | ../../back/amd64/amd64_compiler.exe -o program.asm --no-start - # -vamd64
  > let rec fack n k = if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > let id u = u
  > let main =
  >   let rez = fack 5 id in
  >   let t = print rez in
  >   0
  > EOF

  $ cat program.asm  | grep -v 'section .note.GNU-stack' | nl -ba
       1	section .text
       2	extern rukaml_alloc_closure
       3	extern rukaml_print_int
       4	extern rukaml_applyN
       5	extern rukaml_field
       6	extern rukaml_alloc_pair
       7	extern rukaml_initialize
       8	extern rukaml_gc_compact
       9	extern rukaml_gc_print_stats
      10	extern rukaml_print_alloc_closure_count
      11	
      12	GLOBAL fresh_1
      13	
      14	fresh_1:
      15	  push rbp
      16	  mov  rbp, rsp
      17	  sub rsp, 8*1 ; allocate for local variables temp1
      18	  sub rsp, 8 ; allocate padding for locals
      19	  mov qword r11, [rbp+2*8]
      20	  mov qword r12, [rbp+4*8]
      21	  imul r11, r12
      22	  mov [rbp-1*8], r11
      23	  sub rsp, 8 ; padding
      24	  sub rsp, 8 ; first arg of a function k
      25	  mov qword rdx, [rbp-1*8] ; use temp rdx to move from stack to stack
      26	  mov qword [rbp-4*8], rdx ; access a var "temp1"
      27	  mov rax, 0  ; no float arguments
      28	  mov rdi, [rbp+3*8]
      29	  mov rsi, 1
      30	  mov rdx, [rbp-4*8]
      31	  call rukaml_applyN
      32	  add rsp, 8*2 ; free space for args of function "k"
      33	  mov rax, rax
      34	  add rsp, 8 ; deallocate padding for locals
      35	  add rsp, 8*1 ; deallocate local variables temp1
      36	  pop rbp
      37	  ret  ;;;; fresh_1
      38	GLOBAL fack
      39	fack:
      40	  push rbp
      41	  mov  rbp, rsp
      42	  sub rsp, 8*5 ; allocate for local variables temp8, temp7, temp6, temp5, temp3
      43	  sub rsp, 8 ; allocate padding for locals
      44	  mov qword r11, [rbp+2*8]
      45	  mov qword r12, 1
      46	  cmp r11, r12
      47	  je lab_16
      48	  mov qword [rbp-1*8], 0
      49	  jmp lab_17
      50	lab_16:
      51	  mov qword [rbp-1*8], 1
      52	  jmp lab_17
      53	lab_17:
      54	  mov qword rdx, [rbp-1*8]
      55	  cmp rdx, 0
      56	  je lab_then_18
      57	  sub rsp, 8 ; padding
      58	  sub rsp, 8 ; first arg of a function k
      59	  mov qword [rbp-8*8],  1
      60	  mov rax, 0  ; no float arguments
      61	  mov rdi, [rbp+3*8]
      62	  mov rsi, 1
      63	  mov rdx, [rbp-8*8]
      64	  call rukaml_applyN
      65	  add rsp, 8*2 ; free space for args of function "k"
      66	  mov rax, rax
      67	  jmp lab_endif_19
      68	lab_then_18:
      69	  mov qword r11, [rbp+2*8]
      70	  dec r11
      71	  mov qword [rbp-2*8], r11
      72	  sub rsp, 8 ; trying to save alignment 16 bytes
      73	  sub rsp, 8*1 ; fun arguments
      74	  mov qword r8, [rbp-2*8]  ; arg "temp5"
      75	  mov qword [rsp+0*8], r8
      76	  mov rdi, fack
      77	  mov rsi, 2
      78	  call rukaml_alloc_closure
      79	  mov rdi, rax
      80	  mov rsi, 1
      81	  mov rdx, [rsp+8*0]
      82	  mov al, 0
      83	  call rukaml_applyN
      84	  add rsp, 8*2 ; deallocate args of rukaml_applyN
      85	  mov [rbp-3*8], rax
      86	  sub rsp, 8 ; trying to save alignment 16 bytes
      87	  sub rsp, 8*1 ; fun arguments
      88	  mov qword r8, [rbp+2*8]  ; arg "n"
      89	  mov qword [rsp+0*8], r8
      90	  mov rdi, fresh_1
      91	  mov rsi, 3
      92	  call rukaml_alloc_closure
      93	  mov rdi, rax
      94	  mov rsi, 1
      95	  mov rdx, [rsp+8*0]
      96	  mov al, 0
      97	  call rukaml_applyN
      98	  add rsp, 8*2 ; deallocate args of rukaml_applyN
      99	  mov [rbp-4*8], rax
     100	  sub rsp, 8 ; padding
     101	  sub rsp, 8 ; first arg of a function temp7
     102	  mov qword rdx, [rbp+3*8] ; use temp rdx to move from stack to stack
     103	  mov qword [rbp-8*8], rdx ; access a var "k"
     104	  mov rax, 0  ; no float arguments
     105	  mov rdi, [rbp-4*8]
     106	  mov rsi, 1
     107	  mov rdx, [rbp-8*8]
     108	  call rukaml_applyN
     109	  add rsp, 8*2 ; free space for args of function "temp7"
     110	  mov [rbp-5*8], rax
     111	  sub rsp, 8 ; padding
     112	  sub rsp, 8 ; first arg of a function temp6
     113	  mov qword rdx, [rbp-5*8] ; use temp rdx to move from stack to stack
     114	  mov qword [rbp-8*8], rdx ; access a var "temp8"
     115	  mov rax, 0  ; no float arguments
     116	  mov rdi, [rbp-3*8]
     117	  mov rsi, 1
     118	  mov rdx, [rbp-8*8]
     119	  call rukaml_applyN
     120	  add rsp, 8*2 ; free space for args of function "temp6"
     121	  mov rax, rax
     122	lab_endif_19:
     123	  add rsp, 8 ; deallocate padding for locals
     124	  add rsp, 8*5 ; deallocate local variables temp8, temp7, temp6, temp5, temp3
     125	  pop rbp
     126	  ret  ;;;; fack
     127	GLOBAL id
     128	id:
     129	  push rbp
     130	  mov  rbp, rsp
     131	  mov qword rdx, [rbp+2*8] ; use temp rdx to move from stack to stack
     132	  mov qword rax, rdx ; access a var "u"
     133	  pop rbp
     134	  ret  ;;;; id
     135	GLOBAL main
     136	main:
     137	  push rbp
     138	  mov  rbp, rsp
     139	  mov rdi, rsp
     140	  call rukaml_initialize
     141	  sub rsp, 8*3 ; allocate for local variables t, rez, temp11
     142	  sub rsp, 8 ; allocate padding for locals
     143	  sub rsp, 8 ; trying to save alignment 16 bytes
     144	  sub rsp, 8*1 ; fun arguments
     145	  mov qword [rsp+0*8], 5 ; constant
     146	  mov rdi, fack
     147	  mov rsi, 2
     148	  call rukaml_alloc_closure
     149	  mov rdi, rax
     150	  mov rsi, 1
     151	  mov rdx, [rsp+8*0]
     152	  mov al, 0
     153	  call rukaml_applyN
     154	  add rsp, 8*2 ; deallocate args of rukaml_applyN
     155	  mov [rbp-1*8], rax
     156	  sub rsp, 8 ; padding
     157	  sub rsp, 8 ; first arg of a function temp11
     158	  mov rdi, id
     159	  mov rsi, 1
     160	  call rukaml_alloc_closure
     161	  mov [rbp-6*8], rax
     162	  mov rax, 0  ; no float arguments
     163	  mov rdi, [rbp-1*8]
     164	  mov rsi, 1
     165	  mov rdx, [rbp-6*8]
     166	  call rukaml_applyN
     167	  add rsp, 8*2 ; free space for args of function "temp11"
     168	  mov [rbp-2*8], rax
     169	  add rsp, -8*2
     170	  mov r11, [rbp-2*8]
     171	  mov qword [rsp], r11
     172	  call rukaml_print_int ; short
     173	  add rsp, 8*2
     174	  mov [rbp-3*8], rax
     175	  mov qword rax,  0
     176	  add rsp, 8 ; deallocate padding for locals
     177	  add rsp, 8*3 ; deallocate local variables t, rez, temp11
     178	  pop rbp
     179	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 program.o ../../back/amd64/rukaml_stdlib.o -o program.exe
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
  $ chmod u+x program.exe && ./program.exe
  rukaml_print_int 120
