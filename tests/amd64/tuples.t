  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 --no-start -
  > let fst p = let (a,b) = p in a
  > let snd p = let (a,b) = p in b
  > let fac3 n =
  >   let store = (3,(2,(1,()))) in
  >   let n3 = fst store in
  >   let n2 = fst (snd store) in
  >   let n1 = fst (snd (snd store)) in
  >   n3 * n2 * n1
  > let main =
  >   let a = 1 in 
  >   let a = 2 in 
  >   let a = 3 in 
  >   let tmp1 = gc_compact () in
  >   let rez1 = fac3 1 in
  >   let t1 = print rez1 in
  >   let alive = (3,(2,(1,()))) in
  >   let tmp2 = gc_compact () in
  >   0
  > EOF
  After ANF transformation.
  let fst p =
    let temp1 = p in
      let a = field 0 temp1 in
        let b = field 1 temp1 in
          a
  let snd p =
    let temp2 = p in
      let a = field 0 temp2 in
        field 1 temp2
  let fac3 n =
    let temp3 = (1, ()) in
      let temp4 = (2, temp3) in
        let temp5 = (3, temp4) in
          let store = temp5 in
            let temp6 = fst store  in
              let n3 = temp6 in
                let temp7 = snd store  in
                  let temp8 = fst temp7  in
                    let n2 = temp8 in
                      let temp9 = snd store  in
                        let temp10 = snd temp9  in
                          let temp11 = fst temp10  in
                            let n1 = temp11 in
                              let temp12 = (n3 * n2) in
                                (temp12 * n1)
  let main =
    let temp14 = fac3 1  in
      let rez1 = temp14 in
        let temp15 = print rez1  in
          let t1 = temp15 in
            let temp16 = fac3 2  in
              let rez2 = temp16 in
                let temp17 = print rez2  in
                  let t2 = temp17 in
                    let temp18 = gc_compact ()  in
                      let tmp = temp18 in
                        0
  ANF: let fst p =
         let temp1 = p in
           let a = field 0 temp1 in
             let b = field 1 temp1 in
               a
       let snd p =
         let temp2 = p in
           let a = field 0 temp2 in
             field 1 temp2
       let fac3 n =
         let temp3 = (1, ()) in
           let temp4 = (2, temp3) in
             let temp5 = (3, temp4) in
               let store = temp5 in
                 let temp6 = fst store  in
                   let n3 = temp6 in
                     let temp7 = snd store  in
                       let temp8 = fst temp7  in
                         let n2 = temp8 in
                           let temp9 = snd store  in
                             let temp10 = snd temp9  in
                               let temp11 = fst temp10  in
                                 let n1 = temp11 in
                                   let temp12 = (n3 * n2) in
                                     (temp12 * n1)
       let main =
         let temp14 = fac3 1  in
           let rez1 = temp14 in
             let temp15 = print rez1  in
               let t1 = temp15 in
                 let temp16 = fac3 2  in
                   let rez2 = temp16 in
                     let temp17 = print rez2  in
                       let t2 = temp17 in
                         let temp18 = gc_compact ()  in
                           let tmp = temp18 in
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
       9	
      10	
      11		; @[{stack||stack}@]
      12	GLOBAL fst
      13	
      14	fst:
      15	  push rbp
      16	  mov  rbp, rsp
      17	  sub rsp, 8 ; allocate for var "temp1"
      18	  mov rdx, [rsp+3*8] 
      19	  mov [rsp], rdx ; access a var "p"
      20	  sub rsp, 8 ; allocate for var "a"
      21	  mov rdx, [rsp+1*8] 
      22	  mov rsi, rdx ; access a var "temp1"
      23	  mov rdi, 0
      24	  call rukaml_field
      25	  mov [rsp], rax
      26	  sub rsp, 8 ; allocate for var "b"
      27	  mov rdx, [rsp+2*8] 
      28	  mov rsi, rdx ; access a var "temp1"
      29	  mov rdi, 1
      30	  call rukaml_field
      31	  mov [rsp], rax
      32	  mov rdx, [rsp+1*8] 
      33	  mov rax, rdx ; access a var "a"
      34	  add rsp, 8 ; deallocate var "b"
      35	  add rsp, 8 ; deallocate var "a"
      36	  add rsp, 8 ; deallocate var "temp1"
      37	  pop rbp
      38	  ret  ;;;; fst
      39	
      40		; @[{stack||stack}@]
      41	GLOBAL snd
      42	snd:
      43	  push rbp
      44	  mov  rbp, rsp
      45	  sub rsp, 8 ; allocate for var "temp2"
      46	  mov rdx, [rsp+3*8] 
      47	  mov [rsp], rdx ; access a var "p"
      48	  sub rsp, 8 ; allocate for var "a"
      49	  mov rdx, [rsp+1*8] 
      50	  mov rsi, rdx ; access a var "temp2"
      51	  mov rdi, 0
      52	  call rukaml_field
      53	  mov [rsp], rax
      54	  mov rdx, [rsp+1*8] 
      55	  mov rsi, rdx ; access a var "temp2"
      56	  mov rdi, 1
      57	  call rukaml_field
      58	  mov rax, rax
      59	  add rsp, 8 ; deallocate var "a"
      60	  add rsp, 8 ; deallocate var "temp2"
      61	  pop rbp
      62	  ret  ;;;; snd
      63	
      64		; @[{stack||stack}@]
      65	GLOBAL fac3
      66	fac3:
      67	  push rbp
      68	  mov  rbp, rsp
      69	  sub rsp, 8 ; allocate for var "temp3"
      70	  mov qword rdi,  1
      71	mov qword rsi, 0
      72	  call rukaml_alloc_pair
      73	  mov [rsp], rax
      74	  sub rsp, 8 ; allocate for var "temp4"
      75	  mov qword rdi,  2
      76	  mov rdx, [rsp+1*8] 
      77	  mov rsi, rdx ; access a var "temp3"
      78	  call rukaml_alloc_pair
      79	  mov [rsp], rax
      80	  sub rsp, 8 ; allocate for var "temp5"
      81	  mov qword rdi,  3
      82	  mov rdx, [rsp+1*8] 
      83	  mov rsi, rdx ; access a var "temp4"
      84	  call rukaml_alloc_pair
      85	  mov [rsp], rax
      86	  sub rsp, 8 ; allocate for var "store"
      87	  mov rdx, [rsp+1*8] 
      88	  mov [rsp], rdx ; access a var "temp5"
      89	  sub rsp, 8 ; allocate for var "temp6"
      90		; expected_arity = 1
      91		; formal_arity = 1
      92		; calling "fst"
      93	  ; expected_arity = formal_arity = 1
      94	  sub rsp, 8 ; allocate for argument 0 (name = __temp7)
      95	  mov rdx, [rsp+2*8] 
      96	  mov [rsp], rdx ; access a var "store"
      97	  call fst
      98	  add rsp, 8 ; deallocate var "__temp7"
      99	  mov [rsp], rax
     100	  sub rsp, 8 ; allocate for var "n3"
     101	  mov rdx, [rsp+1*8] 
     102	  mov [rsp], rdx ; access a var "temp6"
     103	  sub rsp, 8 ; allocate for var "temp7"
     104		; expected_arity = 1
     105		; formal_arity = 1
     106		; calling "snd"
     107	  ; expected_arity = formal_arity = 1
     108	  sub rsp, 8 ; allocate for argument 0 (name = __temp8)
     109	  mov rdx, [rsp+4*8] 
     110	  mov [rsp], rdx ; access a var "store"
     111	  call snd
     112	  add rsp, 8 ; deallocate var "__temp8"
     113	  mov [rsp], rax
     114	  sub rsp, 8 ; allocate for var "temp8"
     115		; expected_arity = 1
     116		; formal_arity = 1
     117		; calling "fst"
     118	  ; expected_arity = formal_arity = 1
     119	  sub rsp, 8 ; allocate for argument 0 (name = __temp9)
     120	  mov rdx, [rsp+2*8] 
     121	  mov [rsp], rdx ; access a var "temp7"
     122	  call fst
     123	  add rsp, 8 ; deallocate var "__temp9"
     124	  mov [rsp], rax
     125	  sub rsp, 8 ; allocate for var "n2"
     126	  mov rdx, [rsp+1*8] 
     127	  mov [rsp], rdx ; access a var "temp8"
     128	  sub rsp, 8 ; allocate for var "temp9"
     129		; expected_arity = 1
     130		; formal_arity = 1
     131		; calling "snd"
     132	  ; expected_arity = formal_arity = 1
     133	  sub rsp, 8 ; allocate for argument 0 (name = __temp10)
     134	  mov rdx, [rsp+7*8] 
     135	  mov [rsp], rdx ; access a var "store"
     136	  call snd
     137	  add rsp, 8 ; deallocate var "__temp10"
     138	  mov [rsp], rax
     139	  sub rsp, 8 ; allocate for var "temp10"
     140		; expected_arity = 1
     141		; formal_arity = 1
     142		; calling "snd"
     143	  ; expected_arity = formal_arity = 1
     144	  sub rsp, 8 ; allocate for argument 0 (name = __temp11)
     145	  mov rdx, [rsp+2*8] 
     146	  mov [rsp], rdx ; access a var "temp9"
     147	  call snd
     148	  add rsp, 8 ; deallocate var "__temp11"
     149	  mov [rsp], rax
     150	  sub rsp, 8 ; allocate for var "temp11"
     151		; expected_arity = 1
     152		; formal_arity = 1
     153		; calling "fst"
     154	  ; expected_arity = formal_arity = 1
     155	  sub rsp, 8 ; allocate for argument 0 (name = __temp12)
     156	  mov rdx, [rsp+2*8] 
     157	  mov [rsp], rdx ; access a var "temp10"
     158	  call fst
     159	  add rsp, 8 ; deallocate var "__temp12"
     160	  mov [rsp], rax
     161	  sub rsp, 8 ; allocate for var "n1"
     162	  mov rdx, [rsp+1*8] 
     163	  mov [rsp], rdx ; access a var "temp11"
     164	  sub rsp, 8 ; allocate for var "temp12"
     165	  sub rsp, 8 ; allocate for var "__temp13"
     166	  mov rdx, [rsp+9*8] 
     167	  mov [rsp], rdx ; access a var "n3"
     168	  sub rsp, 8 ; allocate for var "__temp14"
     169	  mov rdx, [rsp+7*8] 
     170	  mov [rsp], rdx ; access a var "n2"
     171	  mov rax, [8*1+rsp]
     172	  mov rbx, [rsp]
     173	  imul rbx, rax
     174	  mov [8*2+rsp], rbx
     175	  add rsp, 8 ; deallocate var "__temp14"
     176	  add rsp, 8 ; deallocate var "__temp13"
     177	  sub rsp, 8 ; allocate for var "__temp15"
     178	  mov rdx, [rsp+1*8] 
     179	  mov [rsp], rdx ; access a var "temp12"
     180	  sub rsp, 8 ; allocate for var "__temp16"
     181	  mov rdx, [rsp+3*8] 
     182	  mov [rsp], rdx ; access a var "n1"
     183	  mov rax, [8*1+rsp]
     184	  mov rbx, [rsp]
     185	  imul rbx, rax
     186	  mov rax, rbx
     187	  add rsp, 8 ; deallocate var "__temp16"
     188	  add rsp, 8 ; deallocate var "__temp15"
     189	  add rsp, 8 ; deallocate var "temp12"
     190	  add rsp, 8 ; deallocate var "n1"
     191	  add rsp, 8 ; deallocate var "temp11"
     192	  add rsp, 8 ; deallocate var "temp10"
     193	  add rsp, 8 ; deallocate var "temp9"
     194	  add rsp, 8 ; deallocate var "n2"
     195	  add rsp, 8 ; deallocate var "temp8"
     196	  add rsp, 8 ; deallocate var "temp7"
     197	  add rsp, 8 ; deallocate var "n3"
     198	  add rsp, 8 ; deallocate var "temp6"
     199	  add rsp, 8 ; deallocate var "store"
     200	  add rsp, 8 ; deallocate var "temp5"
     201	  add rsp, 8 ; deallocate var "temp4"
     202	  add rsp, 8 ; deallocate var "temp3"
     203	  pop rbp
     204	  ret  ;;;; fac3
     205	
     206		; @[{stack||stack}@]
     207	GLOBAL main
     208	main:
     209	  push rbp
     210	  mov  rbp, rsp
     211	mov rdi, [rsp]
     212	call rukaml_initialize
     213	  sub rsp, 8 ; allocate for var "temp14"
     214		; expected_arity = 1
     215		; formal_arity = 1
     216		; calling "fac3"
     217	  ; expected_arity = formal_arity = 1
     218	  sub rsp, 8 ; allocate for argument 0 (name = __temp19)
     219	  mov qword [rsp],  1
     220	  call fac3
     221	  add rsp, 8 ; deallocate var "__temp19"
     222	  mov [rsp], rax
     223	  sub rsp, 8 ; allocate for var "rez1"
     224	  mov rdx, [rsp+1*8] 
     225	  mov [rsp], rdx ; access a var "temp14"
     226	  sub rsp, 8 ; allocate for var "temp15"
     227	  mov rdi, [8*1+rsp]
     228	  call rukaml_print_int ; short
     229	  mov [rsp], rax
     230	  sub rsp, 8 ; allocate for var "t1"
     231	  mov rdx, [rsp+1*8] 
     232	  mov [rsp], rdx ; access a var "temp15"
     233	  sub rsp, 8 ; allocate for var "temp16"
     234		; expected_arity = 1
     235		; formal_arity = 1
     236		; calling "fac3"
     237	  ; expected_arity = formal_arity = 1
     238	  sub rsp, 8 ; allocate for argument 0 (name = __temp20)
     239	  mov qword [rsp],  2
     240	  call fac3
     241	  add rsp, 8 ; deallocate var "__temp20"
     242	  mov [rsp], rax
     243	  sub rsp, 8 ; allocate for var "rez2"
     244	  mov rdx, [rsp+1*8] 
     245	  mov [rsp], rdx ; access a var "temp16"
     246	  sub rsp, 8 ; allocate for var "temp17"
     247	  mov rdi, [8*1+rsp]
     248	  call rukaml_print_int ; short
     249	  mov [rsp], rax
     250	  sub rsp, 8 ; allocate for var "t2"
     251	  mov rdx, [rsp+1*8] 
     252	  mov [rsp], rdx ; access a var "temp17"
     253	  sub rsp, 8 ; allocate for var "temp18"
     254	  mov rdi, 0
     255	  mov rsi, 0
     256	  call rukaml_gc_compact
     257	  sub rsp, 8 ; allocate for var "tmp"
     258	  mov rdx, [rsp+1*8] 
     259	  mov [rsp], rdx ; access a var "temp18"
     260	  mov qword rax,  0
     261	  add rsp, 8 ; deallocate var "tmp"
     262	  add rsp, 8 ; deallocate var "temp18"
     263	  add rsp, 8 ; deallocate var "t2"
     264	  add rsp, 8 ; deallocate var "temp17"
     265	  add rsp, 8 ; deallocate var "rez2"
     266	  add rsp, 8 ; deallocate var "temp16"
     267	  add rsp, 8 ; deallocate var "t1"
     268	  add rsp, 8 ; deallocate var "temp15"
     269	  add rsp, 8 ; deallocate var "rez1"
     270	  add rsp, 8 ; deallocate var "temp14"
     271	  pop rbp
     272	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc program.o ../../back_amd64/rukaml_stdlib.o -o program.exe 
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  $ chmod u+x program.exe && ./program.exe
  A pair created. Allocated words = 3
  A pair created. Allocated words = 6
  A pair created. Allocated words = 9
  6
  A pair created. Allocated words = 12
  A pair created. Allocated words = 15
  A pair created. Allocated words = 18
  6
