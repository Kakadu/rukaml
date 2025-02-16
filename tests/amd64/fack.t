  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm --no-start - # -vamd64
  > let rec fack n k = if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > let id u = u
  > let main =
  >   let rez = fack 5 id in
  >   let t = print rez in
  >   0
  > EOF
  After ANF transformation.
  let fresh_1 n k m =
    let temp1 = (n * m) in
      k temp1 
  let rec fack n k =
    let temp3 = (n = 1) in
      (if temp3
      then k 1 
      else let temp5 = (n - 1) in
             let temp6 = fack temp5  in
               let temp7 = fresh_1 n  in
                 let temp8 = temp7 k  in
                   temp6 temp8 )
  let id u =
    u
  let main =
    let temp11 = fack 5  in
      let rez = temp11 id  in
        let t = print rez  in
          0

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
      10	
      11	GLOBAL fresh_1
      12	
      13	fresh_1:
      14	  push rbp
      15	  mov  rbp, rsp
      16	  sub rsp, 8*1 ; allocate for local variables temp1
      17	  sub rsp, 8 ; allocate padding for locals
      18	  mov qword r11, [rbp+2*8]
      19	  mov qword r12, [rbp+4*8]
      20	  imul r11, r12
      21	  mov [rbp-1*8], r11
      22	  sub rsp, 8 ; padding
      23	  sub rsp, 8 ; first arg of a function k
      24	  mov qword rdx, [rbp-1*8] ; use temp rdx to move from stack to stack
      25	  mov qword [rbp-4*8], rdx ; access a var "temp1"
      26	  mov rax, 0  ; no float arguments
      27	  mov rdi, [rbp+3*8]
      28	  mov rsi, 1
      29	  mov rdx, [rbp-4*8]
      30	  call rukaml_applyN
      31	  add rsp, 8*2 ; free space for args of function "k"
      32	  mov rax, rax
      33	  add rsp, 8 ; deallocate padding for locals
      34	  add rsp, 8*1 ; deallocate local variables temp1
      35	  pop rbp
      36	  ret  ;;;; fresh_1
      37	GLOBAL fack
      38	fack:
      39	  push rbp
      40	  mov  rbp, rsp
      41	  sub rsp, 8*5 ; allocate for local variables temp8, temp7, temp6, temp5, temp3
      42	  sub rsp, 8 ; allocate padding for locals
      43	  mov qword r11, [rbp+2*8]
      44	  mov qword r12, 1
      45	  cmp r11, r12
      46	  je lab_16
      47	  mov qword [rbp-1*8], 0
      48	  jmp lab_17
      49	lab_16:
      50	  mov qword [rbp-1*8], 1
      51	  jmp lab_17
      52	lab_17:
      53	  mov qword rdx, [rbp-1*8]
      54	  cmp rdx, 0
      55	  je lab_then_18
      56	  sub rsp, 8 ; padding
      57	  sub rsp, 8 ; first arg of a function k
      58	  mov qword [rbp-8*8],  1
      59	  mov rax, 0  ; no float arguments
      60	  mov rdi, [rbp+3*8]
      61	  mov rsi, 1
      62	  mov rdx, [rbp-8*8]
      63	  call rukaml_applyN
      64	  add rsp, 8*2 ; free space for args of function "k"
      65	  mov rax, rax
      66	  jmp lab_endif_19
      67	lab_then_18:
      68	  mov qword r11, [rbp+2*8]
      69	  dec r11
      70	  mov qword [rbp-2*8], r11
      71	  sub rsp, 8 ; trying to save alignment 16 bytes
      72	  sub rsp, 8*1 ; fun arguments
      73	  mov qword r8, [rbp-2*8]  ; arg "temp5"
      74	  mov qword [rsp+0*8], r8
      75	  mov rdi, fack
      76	  mov rsi, 2
      77	  call rukaml_alloc_closure
      78	  mov rdi, rax
      79	  mov rsi, 1
      80	  mov rdx, [rsp+8*0]
      81	  mov al, 0
      82	  call rukaml_applyN
      83	  add rsp, 8*2 ; deallocate args of rukaml_applyN
      84	  mov [rbp-3*8], rax
      85	  sub rsp, 8 ; trying to save alignment 16 bytes
      86	  sub rsp, 8*1 ; fun arguments
      87	  mov qword r8, [rbp+2*8]  ; arg "n"
      88	  mov qword [rsp+0*8], r8
      89	  mov rdi, fresh_1
      90	  mov rsi, 3
      91	  call rukaml_alloc_closure
      92	  mov rdi, rax
      93	  mov rsi, 1
      94	  mov rdx, [rsp+8*0]
      95	  mov al, 0
      96	  call rukaml_applyN
      97	  add rsp, 8*2 ; deallocate args of rukaml_applyN
      98	  mov [rbp-4*8], rax
      99	  sub rsp, 8 ; padding
     100	  sub rsp, 8 ; first arg of a function temp7
     101	  mov qword rdx, [rbp+3*8] ; use temp rdx to move from stack to stack
     102	  mov qword [rbp-8*8], rdx ; access a var "k"
     103	  mov rax, 0  ; no float arguments
     104	  mov rdi, [rbp-4*8]
     105	  mov rsi, 1
     106	  mov rdx, [rbp-8*8]
     107	  call rukaml_applyN
     108	  add rsp, 8*2 ; free space for args of function "temp7"
     109	  mov [rbp-5*8], rax
     110	  sub rsp, 8 ; padding
     111	  sub rsp, 8 ; first arg of a function temp6
     112	  mov qword rdx, [rbp-5*8] ; use temp rdx to move from stack to stack
     113	  mov qword [rbp-8*8], rdx ; access a var "temp8"
     114	  mov rax, 0  ; no float arguments
     115	  mov rdi, [rbp-3*8]
     116	  mov rsi, 1
     117	  mov rdx, [rbp-8*8]
     118	  call rukaml_applyN
     119	  add rsp, 8*2 ; free space for args of function "temp6"
     120	  mov rax, rax
     121	lab_endif_19:
     122	  add rsp, 8 ; deallocate padding for locals
     123	  add rsp, 8*5 ; deallocate local variables temp8, temp7, temp6, temp5, temp3
     124	  pop rbp
     125	  ret  ;;;; fack
     126	GLOBAL id
     127	id:
     128	  push rbp
     129	  mov  rbp, rsp
     130	  mov qword rdx, [rbp+2*8] ; use temp rdx to move from stack to stack
     131	  mov qword rax, rdx ; access a var "u"
     132	  pop rbp
     133	  ret  ;;;; id
     134	GLOBAL main
     135	main:
     136	  push rbp
     137	  mov  rbp, rsp
     138	  mov rdi, rsp
     139	  call rukaml_initialize
     140	  sub rsp, 8*3 ; allocate for local variables t, rez, temp11
     141	  sub rsp, 8 ; allocate padding for locals
     142	  sub rsp, 8 ; trying to save alignment 16 bytes
     143	  sub rsp, 8*1 ; fun arguments
     144	  mov qword [rsp+0*8], 5 ; constant
     145	  mov rdi, fack
     146	  mov rsi, 2
     147	  call rukaml_alloc_closure
     148	  mov rdi, rax
     149	  mov rsi, 1
     150	  mov rdx, [rsp+8*0]
     151	  mov al, 0
     152	  call rukaml_applyN
     153	  add rsp, 8*2 ; deallocate args of rukaml_applyN
     154	  mov [rbp-1*8], rax
     155	  sub rsp, 8 ; padding
     156	  sub rsp, 8 ; first arg of a function temp11
     157	  mov rdi, id
     158	  mov rsi, 1
     159	  call rukaml_alloc_closure
     160	  mov [rbp-6*8], rax
     161	  mov rax, 0  ; no float arguments
     162	  mov rdi, [rbp-1*8]
     163	  mov rsi, 1
     164	  mov rdx, [rbp-6*8]
     165	  call rukaml_applyN
     166	  add rsp, 8*2 ; free space for args of function "temp11"
     167	  mov [rbp-2*8], rax
     168	  add rsp, -8*2
     169	  mov r11, [rbp-2*8]
     170	  mov qword [rsp], r11
     171	  call rukaml_print_int ; short
     172	  add rsp, 8*2
     173	  mov [rbp-3*8], rax
     174	  mov qword rax,  0
     175	  add rsp, 8 ; deallocate padding for locals
     176	  add rsp, 8*3 ; deallocate local variables t, rez, temp11
     177	  pop rbp
     178	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
  $ chmod u+x program.exe && ./program.exe
  rukaml_print_int 120
