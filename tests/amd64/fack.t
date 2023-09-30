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
      let rez = temp11 id  in
        let t = print rez  in
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
           let rez = temp11 id  in
             let t = print rez  in
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
      11	GLOBAL fresh_1
      12	
      13	fresh_1:
      14	  push rbp
      15	  mov  rbp, rsp
      16	  sub rsp, 8 ; allocate for var "temp1"
      17	  sub rsp, 8 ; allocate for var "__temp3"
      18	  mov rdx, [rsp+4*8] 
      19	  mov [rsp], rdx ; access a var "n"
      20	  sub rsp, 8 ; allocate for var "__temp4"
      21	  mov rdx, [rsp+7*8] 
      22	  mov [rsp], rdx ; access a var "m"
      23	  mov rax, [8*1+rsp]
      24	  mov rbx, [rsp]
      25	  imul rbx, rax
      26	  mov [8*2+rsp], rbx
      27	  add rsp, 8 ; deallocate var "__temp4"
      28	  add rsp, 8 ; deallocate var "__temp3"
      29	  sub rsp, 8 ; allocate for var "__temp5"
      30	  mov rdx, [rsp+1*8] 
      31	  mov [rsp], rdx ; access a var "temp1"
      32	  mov rax, 0  ; no float arguments
      33	  mov rdi, [8*5+rsp]
      34	  mov rsi, 1
      35	  mov rdx, [rsp]
      36	  call rukaml_applyN
      37	  add rsp, 8 ; deallocate var "__temp5"
      38	  mov rax, rax
      39	  add rsp, 8 ; deallocate var "temp1"
      40	  pop rbp
      41	  ret  ;;;; fresh_1
      42	GLOBAL fack
      43	fack:
      44	  push rbp
      45	  mov  rbp, rsp
      46	  sub rsp, 8 ; allocate for var "temp3"
      47	  sub rsp, 8 ; allocate for var "__temp8"
      48	  mov rdx, [rsp+4*8] 
      49	  mov [rsp], rdx ; access a var "n"
      50	  sub rsp, 8 ; allocate for var "__temp9"
      51	  mov qword [rsp],  1
      52	  mov rax, [8*1+rsp]
      53	  mov rbx, [rsp]
      54	  cmp rax, rbx
      55	  je lab_14
      56	  mov qword [8*2+rsp], 0
      57	  jmp lab_15
      58	lab_14:
      59	  mov qword [8*2+rsp], 1
      60	  jmp lab_15
      61	lab_15:
      62	  add rsp, 8 ; deallocate var "__temp9"
      63	  add rsp, 8 ; deallocate var "__temp8"
      64	  mov rdx, [rsp+0*8] 
      65	  cmp rdx, 0
      66	  je lab_then_16
      67	  sub rsp, 8 ; allocate for var "__temp10"
      68	  mov qword [rsp],  1
      69	  mov rax, 0  ; no float arguments
      70	  mov rdi, [8*5+rsp]
      71	  mov rsi, 1
      72	  mov rdx, [rsp]
      73	  call rukaml_applyN
      74	  add rsp, 8 ; deallocate var "__temp10"
      75	  mov rax, rax
      76	  jmp lab_endif_17
      77	  lab_then_16:
      78	  sub rsp, 8 ; allocate for var "temp5"
      79	  sub rsp, 8 ; allocate for var "__temp11"
      80	  mov rdx, [rsp+5*8] 
      81	  mov [rsp], rdx ; access a var "n"
      82	  mov rax, [rsp]
      83	  dec rax
      84	  mov [8*1+rsp], rax
      85	  add rsp, 8 ; deallocate var "__temp11"
      86	  sub rsp, 8 ; allocate for var "temp6"
      87		; expected_arity = 2
      88		; formal_arity = 1
      89		; calling "fack"
      90	  sub rsp, 8 ; allocate wrapper for func __temp12
      91	  mov rdi, fack
      92	  mov rsi, 2
      93	  call rukaml_alloc_closure
      94	  mov [rsp], rax
      95	  sub rsp, 8 ; allocate for argument 0 (name = __temp13)
      96	  mov rdx, [rsp+3*8] 
      97	  mov [rsp], rdx ; access a var "temp5"
      98	  mov rdi, [8*1+rsp]
      99	  mov rsi, 1
     100	  mov rdx, [rsp]
     101	  mov al, 0
     102	  call rukaml_applyN
     103	  mov [8*2+rsp], rax
     104	  add rsp, 8 ; deallocate var "__temp13"
     105	  add rsp, 8 ; deallocate var "__temp12"
     106	  sub rsp, 8 ; allocate for var "temp7"
     107		; expected_arity = 3
     108		; formal_arity = 1
     109		; calling "fresh_1"
     110	  sub rsp, 8 ; allocate wrapper for func __temp14
     111	  mov rdi, fresh_1
     112	  mov rsi, 3
     113	  call rukaml_alloc_closure
     114	  mov [rsp], rax
     115	  sub rsp, 8 ; allocate for argument 0 (name = __temp15)
     116	  mov rdx, [rsp+8*8] 
     117	  mov [rsp], rdx ; access a var "n"
     118	  mov rdi, [8*1+rsp]
     119	  mov rsi, 1
     120	  mov rdx, [rsp]
     121	  mov al, 0
     122	  call rukaml_applyN
     123	  mov [8*2+rsp], rax
     124	  add rsp, 8 ; deallocate var "__temp15"
     125	  add rsp, 8 ; deallocate var "__temp14"
     126	  sub rsp, 8 ; allocate for var "temp8"
     127	  sub rsp, 8 ; allocate for var "__temp16"
     128	  mov rdx, [rsp+9*8] 
     129	  mov [rsp], rdx ; access a var "k"
     130	  mov rax, 0  ; no float arguments
     131	  mov rdi, [8*2+rsp]
     132	  mov rsi, 1
     133	  mov rdx, [rsp]
     134	  call rukaml_applyN
     135	  add rsp, 8 ; deallocate var "__temp16"
     136	  mov [rsp], rax
     137	  sub rsp, 8 ; allocate for var "__temp17"
     138	  mov rdx, [rsp+1*8] 
     139	  mov [rsp], rdx ; access a var "temp8"
     140	  mov rax, 0  ; no float arguments
     141	  mov rdi, [8*3+rsp]
     142	  mov rsi, 1
     143	  mov rdx, [rsp]
     144	  call rukaml_applyN
     145	  add rsp, 8 ; deallocate var "__temp17"
     146	  mov rax, rax
     147	  add rsp, 8 ; deallocate var "temp8"
     148	  add rsp, 8 ; deallocate var "temp7"
     149	  add rsp, 8 ; deallocate var "temp6"
     150	  add rsp, 8 ; deallocate var "temp5"
     151	  lab_endif_17:
     152	  add rsp, 8 ; deallocate var "temp3"
     153	  pop rbp
     154	  ret  ;;;; fack
     155	GLOBAL id
     156	id:
     157	  push rbp
     158	  mov  rbp, rsp
     159	  mov rdx, [rsp+2*8] 
     160	  mov rax, rdx ; access a var "u"
     161	  pop rbp
     162	  ret  ;;;; id
     163	GLOBAL main
     164	main:
     165	  push rbp
     166	  mov  rbp, rsp
     167	  mov rdi, rsp
     168	  call rukaml_initialize
     169	  sub rsp, 8 ; allocate for var "temp11"
     170		; expected_arity = 2
     171		; formal_arity = 1
     172		; calling "fack"
     173	  sub rsp, 8 ; allocate wrapper for func __temp22
     174	  mov rdi, fack
     175	  mov rsi, 2
     176	  call rukaml_alloc_closure
     177	  mov [rsp], rax
     178	  sub rsp, 8 ; allocate for argument 0 (name = __temp23)
     179	  mov qword [rsp],  5
     180	  mov rdi, [8*1+rsp]
     181	  mov rsi, 1
     182	  mov rdx, [rsp]
     183	  mov al, 0
     184	  call rukaml_applyN
     185	  mov [8*2+rsp], rax
     186	  add rsp, 8 ; deallocate var "__temp23"
     187	  add rsp, 8 ; deallocate var "__temp22"
     188	  sub rsp, 8 ; allocate for var "rez"
     189	  sub rsp, 8 ; allocate for var "__temp24"
     190	  mov rdi, id
     191	  mov rsi, 1
     192	  call rukaml_alloc_closure
     193	  mov [rsp], rax
     194	  mov rax, 0  ; no float arguments
     195	  mov rdi, [8*2+rsp]
     196	  mov rsi, 1
     197	  mov rdx, [rsp]
     198	  call rukaml_applyN
     199	  add rsp, 8 ; deallocate var "__temp24"
     200	  mov [rsp], rax
     201	  sub rsp, 8 ; allocate for var "t"
     202	  mov rdi, [8*1+rsp]
     203	  call rukaml_print_int ; short
     204	  mov [rsp], rax
     205	  mov qword rax,  0
     206	  add rsp, 8 ; deallocate var "t"
     207	  add rsp, 8 ; deallocate var "rez"
     208	  add rsp, 8 ; deallocate var "temp11"
     209	  pop rbp
     210	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc program.o ../../back_amd64/rukaml_stdlib.o -o program.exe 
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
  $ chmod u+x program.exe && ./program.exe
  rukaml_print_int 120
