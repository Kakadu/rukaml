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
       5	
       6	
       7		; @[{stack||stack}@]
       8	GLOBAL fresh_1
       9	
      10	fresh_1:
      11	  push rbp
      12	  mov  rbp, rsp
      13	  sub rsp, 8 ; allocate for var "temp1"
      14	  sub rsp, 8 ; allocate for var "__temp3"
      15	  mov rdx, [rsp+4*8] 
      16	  mov [rsp], rdx ; access a var "n"
      17	  sub rsp, 8 ; allocate for var "__temp4"
      18	  mov rdx, [rsp+7*8] 
      19	  mov [rsp], rdx ; access a var "m"
      20	  mov rax, [8*1+rsp]
      21	  mov rbx, [rsp]
      22	  imul rbx, rax
      23	  mov [8*2+rsp], rbx
      24	  add rsp, 8 ; deallocate var "__temp4"
      25	  add rsp, 8 ; deallocate var "__temp3"
      26	  sub rsp, 8 ; allocate for var "__temp5"
      27	  mov rdx, [rsp+1*8] 
      28	  mov [rsp], rdx ; access a var "temp1"
      29	  mov rax, 0  ; no float arguments
      30	  mov rdi, [8*5+rsp]
      31	  mov rsi, 1
      32	  mov rdx, [rsp]
      33	  call rukaml_applyN
      34	  add rsp, 8 ; deallocate var "__temp5"
      35	  mov rax, rax
      36	  add rsp, 8 ; deallocate var "temp1"
      37	  pop rbp
      38	  ret  ;;;; fresh_1
      39	
      40		; @[{stack||stack}@]
      41	GLOBAL fack
      42	fack:
      43	  push rbp
      44	  mov  rbp, rsp
      45	  sub rsp, 8 ; allocate for var "temp3"
      46	  sub rsp, 8 ; allocate for var "__temp8"
      47	  mov rdx, [rsp+4*8] 
      48	  mov [rsp], rdx ; access a var "n"
      49	  sub rsp, 8 ; allocate for var "__temp9"
      50	  mov qword [rsp],  1
      51	  mov rax, [8*1+rsp]
      52	  mov rbx, [rsp]
      53	  cmp rax, rbx
      54	  je lab_14
      55	  mov qword [8*2+rsp], 0
      56	  jmp lab_15
      57	  lab_14:
      58	    mov qword [8*2+rsp], 1
      59	    jmp lab_15
      60	  lab_15:
      61	  add rsp, 8 ; deallocate var "__temp9"
      62	  add rsp, 8 ; deallocate var "__temp8"
      63	  mov rdx, [rsp+0*8] 
      64	  cmp rdx, 0
      65	  je lab_then_16
      66	  sub rsp, 8 ; allocate for var "__temp10"
      67	  mov qword [rsp],  1
      68	  mov rax, 0  ; no float arguments
      69	  mov rdi, [8*5+rsp]
      70	  mov rsi, 1
      71	  mov rdx, [rsp]
      72	  call rukaml_applyN
      73	  add rsp, 8 ; deallocate var "__temp10"
      74	  mov rax, rax
      75	  jmp lab_endif_17
      76	  lab_then_16:
      77	  sub rsp, 8 ; allocate for var "temp5"
      78	  sub rsp, 8 ; allocate for var "__temp11"
      79	  mov rdx, [rsp+5*8] 
      80	  mov [rsp], rdx ; access a var "n"
      81	  mov rax, [rsp]
      82	  dec rax
      83	  mov [8*1+rsp], rax
      84	  add rsp, 8 ; deallocate var "__temp11"
      85	  sub rsp, 8 ; allocate for var "temp6"
      86		; expected_arity = 2
      87		; formal_arity = 1
      88		; calling "fack"
      89	  sub rsp, 8 ; allocate wrapper for func __temp12
      90	  mov rdi, fack
      91	  mov rsi, 2
      92	  call rukaml_alloc_closure
      93	  mov [rsp], rax
      94	  sub rsp, 8 ; allocate for argument 0 (name = __temp13)
      95	  mov rdx, [rsp+3*8] 
      96	  mov [rsp], rdx ; access a var "temp5"
      97	  mov rdi, [8*1+rsp]
      98	  mov rsi, 1
      99	  mov rdx, [rsp]
     100	  mov al, 0
     101	  call rukaml_applyN
     102	  mov [8*2+rsp], rax
     103	  add rsp, 8 ; deallocate var "__temp13"
     104	  add rsp, 8 ; deallocate var "__temp12"
     105	  sub rsp, 8 ; allocate for var "temp7"
     106		; expected_arity = 3
     107		; formal_arity = 1
     108		; calling "fresh_1"
     109	  sub rsp, 8 ; allocate wrapper for func __temp14
     110	  mov rdi, fresh_1
     111	  mov rsi, 3
     112	  call rukaml_alloc_closure
     113	  mov [rsp], rax
     114	  sub rsp, 8 ; allocate for argument 0 (name = __temp15)
     115	  mov rdx, [rsp+8*8] 
     116	  mov [rsp], rdx ; access a var "n"
     117	  mov rdi, [8*1+rsp]
     118	  mov rsi, 1
     119	  mov rdx, [rsp]
     120	  mov al, 0
     121	  call rukaml_applyN
     122	  mov [8*2+rsp], rax
     123	  add rsp, 8 ; deallocate var "__temp15"
     124	  add rsp, 8 ; deallocate var "__temp14"
     125	  sub rsp, 8 ; allocate for var "temp8"
     126	  sub rsp, 8 ; allocate for var "__temp16"
     127	  mov rdx, [rsp+9*8] 
     128	  mov [rsp], rdx ; access a var "k"
     129	  mov rax, 0  ; no float arguments
     130	  mov rdi, [8*2+rsp]
     131	  mov rsi, 1
     132	  mov rdx, [rsp]
     133	  call rukaml_applyN
     134	  add rsp, 8 ; deallocate var "__temp16"
     135	  mov [rsp], rax
     136	  sub rsp, 8 ; allocate for var "__temp17"
     137	  mov rdx, [rsp+1*8] 
     138	  mov [rsp], rdx ; access a var "temp8"
     139	  mov rax, 0  ; no float arguments
     140	  mov rdi, [8*3+rsp]
     141	  mov rsi, 1
     142	  mov rdx, [rsp]
     143	  call rukaml_applyN
     144	  add rsp, 8 ; deallocate var "__temp17"
     145	  mov rax, rax
     146	  add rsp, 8 ; deallocate var "temp8"
     147	  add rsp, 8 ; deallocate var "temp7"
     148	  add rsp, 8 ; deallocate var "temp6"
     149	  add rsp, 8 ; deallocate var "temp5"
     150	  lab_endif_17:
     151	  add rsp, 8 ; deallocate var "temp3"
     152	  pop rbp
     153	  ret  ;;;; fack
     154	
     155		; @[{stack||stack}@]
     156	GLOBAL id
     157	id:
     158	  push rbp
     159	  mov  rbp, rsp
     160	  mov rdx, [rsp+2*8] 
     161	  mov rax, rdx ; access a var "u"
     162	  pop rbp
     163	  ret  ;;;; id
     164	
     165		; @[{stack||stack}@]
     166	GLOBAL main
     167	main:
     168	  push rbp
     169	  mov  rbp, rsp
     170	  sub rsp, 8 ; allocate for var "temp11"
     171		; expected_arity = 2
     172		; formal_arity = 1
     173		; calling "fack"
     174	  sub rsp, 8 ; allocate wrapper for func __temp22
     175	  mov rdi, fack
     176	  mov rsi, 2
     177	  call rukaml_alloc_closure
     178	  mov [rsp], rax
     179	  sub rsp, 8 ; allocate for argument 0 (name = __temp23)
     180	  mov qword [rsp],  5
     181	  mov rdi, [8*1+rsp]
     182	  mov rsi, 1
     183	  mov rdx, [rsp]
     184	  mov al, 0
     185	  call rukaml_applyN
     186	  mov [8*2+rsp], rax
     187	  add rsp, 8 ; deallocate var "__temp23"
     188	  add rsp, 8 ; deallocate var "__temp22"
     189	  sub rsp, 8 ; allocate for var "temp12"
     190	  sub rsp, 8 ; allocate for var "__temp24"
     191	  mov rdi, id
     192	  mov rsi, 1
     193	  call rukaml_alloc_closure
     194	  mov [rsp], rax
     195	  mov rax, 0  ; no float arguments
     196	  mov rdi, [8*2+rsp]
     197	  mov rsi, 1
     198	  mov rdx, [rsp]
     199	  call rukaml_applyN
     200	  add rsp, 8 ; deallocate var "__temp24"
     201	  mov [rsp], rax
     202	  sub rsp, 8 ; allocate for var "rez"
     203	  mov rdx, [rsp+1*8] 
     204	  mov [rsp], rdx ; access a var "temp12"
     205	  sub rsp, 8 ; allocate for var "temp13"
     206	  mov rdi, [8*1+rsp]
     207	  call rukaml_print_int ; short
     208	  mov [rsp], rax
     209	  sub rsp, 8 ; allocate for var "t"
     210	  mov rdx, [rsp+1*8] 
     211	  mov [rsp], rdx ; access a var "temp13"
     212	  mov qword rax,  0
     213	  add rsp, 8 ; deallocate var "t"
     214	  add rsp, 8 ; deallocate var "temp13"
     215	  add rsp, 8 ; deallocate var "rez"
     216	  add rsp, 8 ; deallocate var "temp12"
     217	  add rsp, 8 ; deallocate var "temp11"
     218	  pop rbp
     219	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc program.o ../../back_amd64/rukaml_stdlib.o -o program.exe 
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
  $ chmod u+x program.exe && ./program.exe
  120
