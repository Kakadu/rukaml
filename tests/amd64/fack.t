  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 --no-start -
  > let rec fack n k = if n=1 then k 1 else fack (n-1) (fun m -> k(n*m))
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
      let temp12 = temp11 id  in
        let rez = temp12 in
          let temp13 = print rez  in
            let t = temp13 in
              0
  ANF: let fresh_1 n k m =
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
           let temp12 = temp11 id  in
             let rez = temp12 in
               let temp13 = print rez  in
                 let t = temp13 in
                   0

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
      11	
      12		; @[{stack||stack}@]
      13	GLOBAL fresh_1
      14	
      15	fresh_1:
      16	  push rbp
      17	  mov  rbp, rsp
      18	  sub rsp, 8 ; allocate for var "temp1"
      19	  sub rsp, 8 ; allocate for var "__temp3"
      20	  mov rdx, [rsp+4*8] 
      21	  mov [rsp], rdx ; access a var "n"
      22	  sub rsp, 8 ; allocate for var "__temp4"
      23	  mov rdx, [rsp+7*8] 
      24	  mov [rsp], rdx ; access a var "m"
      25	  mov rax, [8*1+rsp]
      26	  mov rbx, [rsp]
      27	  imul rbx, rax
      28	  mov [8*2+rsp], rbx
      29	  add rsp, 8 ; deallocate var "__temp4"
      30	  add rsp, 8 ; deallocate var "__temp3"
      31	  sub rsp, 8 ; allocate for var "__temp5"
      32	  mov rdx, [rsp+1*8] 
      33	  mov [rsp], rdx ; access a var "temp1"
      34	  mov rax, 0  ; no float arguments
      35	  mov rdi, [8*5+rsp]
      36	  mov rsi, 1
      37	  mov rdx, [rsp]
      38	  call rukaml_applyN
      39	  add rsp, 8 ; deallocate var "__temp5"
      40	  mov rax, rax
      41	  add rsp, 8 ; deallocate var "temp1"
      42	  pop rbp
      43	  ret  ;;;; fresh_1
      44	
      45		; @[{stack||stack}@]
      46	GLOBAL fack
      47	fack:
      48	  push rbp
      49	  mov  rbp, rsp
      50	  sub rsp, 8 ; allocate for var "temp3"
      51	  sub rsp, 8 ; allocate for var "__temp8"
      52	  mov rdx, [rsp+4*8] 
      53	  mov [rsp], rdx ; access a var "n"
      54	  sub rsp, 8 ; allocate for var "__temp9"
      55	  mov qword [rsp],  1
      56	  mov rax, [8*1+rsp]
      57	  mov rbx, [rsp]
      58	  cmp rax, rbx
      59	  je lab_14
      60	  mov qword [8*2+rsp], 0
      61	  jmp lab_15
      62	  lab_14:
      63	    mov qword [8*2+rsp], 1
      64	    jmp lab_15
      65	  lab_15:
      66	  add rsp, 8 ; deallocate var "__temp9"
      67	  add rsp, 8 ; deallocate var "__temp8"
      68	  mov rdx, [rsp+0*8] 
      69	  cmp rdx, 0
      70	  je lab_then_16
      71	  sub rsp, 8 ; allocate for var "__temp10"
      72	  mov qword [rsp],  1
      73	  mov rax, 0  ; no float arguments
      74	  mov rdi, [8*5+rsp]
      75	  mov rsi, 1
      76	  mov rdx, [rsp]
      77	  call rukaml_applyN
      78	  add rsp, 8 ; deallocate var "__temp10"
      79	  mov rax, rax
      80	  jmp lab_endif_17
      81	  lab_then_16:
      82	  sub rsp, 8 ; allocate for var "temp5"
      83	  sub rsp, 8 ; allocate for var "__temp11"
      84	  mov rdx, [rsp+5*8] 
      85	  mov [rsp], rdx ; access a var "n"
      86	  mov rax, [rsp]
      87	  dec rax
      88	  mov [8*1+rsp], rax
      89	  add rsp, 8 ; deallocate var "__temp11"
      90	  sub rsp, 8 ; allocate for var "temp6"
      91		; expected_arity = 2
      92		; formal_arity = 1
      93		; calling "fack"
      94	  sub rsp, 8 ; allocate wrapper for func __temp12
      95	  mov rdi, fack
      96	  mov rsi, 2
      97	  call rukaml_alloc_closure
      98	  mov [rsp], rax
      99	  sub rsp, 8 ; allocate for argument 0 (name = __temp13)
     100	  mov rdx, [rsp+3*8] 
     101	  mov [rsp], rdx ; access a var "temp5"
     102	  mov rdi, [8*1+rsp]
     103	  mov rsi, 1
     104	  mov rdx, [rsp]
     105	  mov al, 0
     106	  call rukaml_applyN
     107	  mov [8*2+rsp], rax
     108	  add rsp, 8 ; deallocate var "__temp13"
     109	  add rsp, 8 ; deallocate var "__temp12"
     110	  sub rsp, 8 ; allocate for var "temp7"
     111		; expected_arity = 3
     112		; formal_arity = 1
     113		; calling "fresh_1"
     114	  sub rsp, 8 ; allocate wrapper for func __temp14
     115	  mov rdi, fresh_1
     116	  mov rsi, 3
     117	  call rukaml_alloc_closure
     118	  mov [rsp], rax
     119	  sub rsp, 8 ; allocate for argument 0 (name = __temp15)
     120	  mov rdx, [rsp+8*8] 
     121	  mov [rsp], rdx ; access a var "n"
     122	  mov rdi, [8*1+rsp]
     123	  mov rsi, 1
     124	  mov rdx, [rsp]
     125	  mov al, 0
     126	  call rukaml_applyN
     127	  mov [8*2+rsp], rax
     128	  add rsp, 8 ; deallocate var "__temp15"
     129	  add rsp, 8 ; deallocate var "__temp14"
     130	  sub rsp, 8 ; allocate for var "temp8"
     131	  sub rsp, 8 ; allocate for var "__temp16"
     132	  mov rdx, [rsp+9*8] 
     133	  mov [rsp], rdx ; access a var "k"
     134	  mov rax, 0  ; no float arguments
     135	  mov rdi, [8*2+rsp]
     136	  mov rsi, 1
     137	  mov rdx, [rsp]
     138	  call rukaml_applyN
     139	  add rsp, 8 ; deallocate var "__temp16"
     140	  mov [rsp], rax
     141	  sub rsp, 8 ; allocate for var "__temp17"
     142	  mov rdx, [rsp+1*8] 
     143	  mov [rsp], rdx ; access a var "temp8"
     144	  mov rax, 0  ; no float arguments
     145	  mov rdi, [8*3+rsp]
     146	  mov rsi, 1
     147	  mov rdx, [rsp]
     148	  call rukaml_applyN
     149	  add rsp, 8 ; deallocate var "__temp17"
     150	  mov rax, rax
     151	  add rsp, 8 ; deallocate var "temp8"
     152	  add rsp, 8 ; deallocate var "temp7"
     153	  add rsp, 8 ; deallocate var "temp6"
     154	  add rsp, 8 ; deallocate var "temp5"
     155	  lab_endif_17:
     156	  add rsp, 8 ; deallocate var "temp3"
     157	  pop rbp
     158	  ret  ;;;; fack
     159	
     160		; @[{stack||stack}@]
     161	GLOBAL id
     162	id:
     163	  push rbp
     164	  mov  rbp, rsp
     165	  mov rdx, [rsp+2*8] 
     166	  mov rax, rdx ; access a var "u"
     167	  pop rbp
     168	  ret  ;;;; id
     169	
     170		; @[{stack||stack}@]
     171	GLOBAL main
     172	main:
     173	  push rbp
     174	  mov  rbp, rsp
     175	mov rdi, rsp
     176	call rukaml_initialize
     177	  sub rsp, 8 ; allocate for var "temp11"
     178		; expected_arity = 2
     179		; formal_arity = 1
     180		; calling "fack"
     181	  sub rsp, 8 ; allocate wrapper for func __temp22
     182	  mov rdi, fack
     183	  mov rsi, 2
     184	  call rukaml_alloc_closure
     185	  mov [rsp], rax
     186	  sub rsp, 8 ; allocate for argument 0 (name = __temp23)
     187	  mov qword [rsp],  5
     188	  mov rdi, [8*1+rsp]
     189	  mov rsi, 1
     190	  mov rdx, [rsp]
     191	  mov al, 0
     192	  call rukaml_applyN
     193	  mov [8*2+rsp], rax
     194	  add rsp, 8 ; deallocate var "__temp23"
     195	  add rsp, 8 ; deallocate var "__temp22"
     196	  sub rsp, 8 ; allocate for var "temp12"
     197	  sub rsp, 8 ; allocate for var "__temp24"
     198	  mov rdi, id
     199	  mov rsi, 1
     200	  call rukaml_alloc_closure
     201	  mov [rsp], rax
     202	  mov rax, 0  ; no float arguments
     203	  mov rdi, [8*2+rsp]
     204	  mov rsi, 1
     205	  mov rdx, [rsp]
     206	  call rukaml_applyN
     207	  add rsp, 8 ; deallocate var "__temp24"
     208	  mov [rsp], rax
     209	  sub rsp, 8 ; allocate for var "rez"
     210	  mov rdx, [rsp+1*8] 
     211	  mov [rsp], rdx ; access a var "temp12"
     212	  sub rsp, 8 ; allocate for var "temp13"
     213	  mov rdi, [8*1+rsp]
     214	  call rukaml_print_int ; short
     215	  mov [rsp], rax
     216	  sub rsp, 8 ; allocate for var "t"
     217	  mov rdx, [rsp+1*8] 
     218	  mov [rsp], rdx ; access a var "temp13"
     219	  mov qword rax,  0
     220	  add rsp, 8 ; deallocate var "t"
     221	  add rsp, 8 ; deallocate var "temp13"
     222	  add rsp, 8 ; deallocate var "rez"
     223	  add rsp, 8 ; deallocate var "temp12"
     224	  add rsp, 8 ; deallocate var "temp11"
     225	  pop rbp
     226	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc program.o ../../back_amd64/rukaml_stdlib.o -o program.exe 
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
  $ chmod u+x program.exe && ./program.exe
  120
