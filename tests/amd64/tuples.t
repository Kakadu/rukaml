>   let rez1 = fac3 1 in
>   let t1 = print rez1 in
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
  >   let alive = (3,(2,(1,()))) in
  >   let tmp1 = fac3 0 in
  >   let tmp2 = gc_compact () in
  >   let tmp4 = gc_stats () in
  >   let tmpl = print 42 in
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
    let temp14 = (1, ()) in
      let temp15 = (2, temp14) in
        let temp16 = (3, temp15) in
          let alive = temp16 in
            let temp17 = fac3 0  in
              let tmp1 = temp17 in
                let temp18 = gc_compact ()  in
                  let tmp2 = temp18 in
                    let temp19 = gc_stats ()  in
                      let tmp4 = temp19 in
                        let temp20 = print 42  in
                          let tmpl = temp20 in
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
         let temp14 = (1, ()) in
           let temp15 = (2, temp14) in
             let temp16 = (3, temp15) in
               let alive = temp16 in
                 let temp17 = fac3 0  in
                   let tmp1 = temp17 in
                     let temp18 = gc_compact ()  in
                       let tmp2 = temp18 in
                         let temp19 = gc_stats ()  in
                           let tmp4 = temp19 in
                             let temp20 = print 42  in
                               let tmpl = temp20 in
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
      13	GLOBAL fst
      14	
      15	fst:
      16	  push rbp
      17	  mov  rbp, rsp
      18	  sub rsp, 8 ; allocate for var "temp1"
      19	  mov rdx, [rsp+3*8] 
      20	  mov [rsp], rdx ; access a var "p"
      21	  sub rsp, 8 ; allocate for var "a"
      22	  mov rdx, [rsp+1*8] 
      23	  mov rsi, rdx ; access a var "temp1"
      24	  mov rdi, 0
      25	  call rukaml_field
      26	  mov [rsp], rax
      27	  sub rsp, 8 ; allocate for var "b"
      28	  mov rdx, [rsp+2*8] 
      29	  mov rsi, rdx ; access a var "temp1"
      30	  mov rdi, 1
      31	  call rukaml_field
      32	  mov [rsp], rax
      33	  mov rdx, [rsp+1*8] 
      34	  mov rax, rdx ; access a var "a"
      35	  add rsp, 8 ; deallocate var "b"
      36	  add rsp, 8 ; deallocate var "a"
      37	  add rsp, 8 ; deallocate var "temp1"
      38	  pop rbp
      39	  ret  ;;;; fst
      40	
      41		; @[{stack||stack}@]
      42	GLOBAL snd
      43	snd:
      44	  push rbp
      45	  mov  rbp, rsp
      46	  sub rsp, 8 ; allocate for var "temp2"
      47	  mov rdx, [rsp+3*8] 
      48	  mov [rsp], rdx ; access a var "p"
      49	  sub rsp, 8 ; allocate for var "a"
      50	  mov rdx, [rsp+1*8] 
      51	  mov rsi, rdx ; access a var "temp2"
      52	  mov rdi, 0
      53	  call rukaml_field
      54	  mov [rsp], rax
      55	  mov rdx, [rsp+1*8] 
      56	  mov rsi, rdx ; access a var "temp2"
      57	  mov rdi, 1
      58	  call rukaml_field
      59	  mov rax, rax
      60	  add rsp, 8 ; deallocate var "a"
      61	  add rsp, 8 ; deallocate var "temp2"
      62	  pop rbp
      63	  ret  ;;;; snd
      64	
      65		; @[{stack||stack}@]
      66	GLOBAL fac3
      67	fac3:
      68	  push rbp
      69	  mov  rbp, rsp
      70	  sub rsp, 8 ; allocate for var "temp3"
      71	  mov qword rdi,  1
      72	mov qword rsi, 0
      73	  call rukaml_alloc_pair
      74	  mov [rsp], rax
      75	  sub rsp, 8 ; allocate for var "temp4"
      76	  mov qword rdi,  2
      77	  mov rdx, [rsp+1*8] 
      78	  mov rsi, rdx ; access a var "temp3"
      79	  call rukaml_alloc_pair
      80	  mov [rsp], rax
      81	  sub rsp, 8 ; allocate for var "temp5"
      82	  mov qword rdi,  3
      83	  mov rdx, [rsp+1*8] 
      84	  mov rsi, rdx ; access a var "temp4"
      85	  call rukaml_alloc_pair
      86	  mov [rsp], rax
      87	  sub rsp, 8 ; allocate for var "store"
      88	  mov rdx, [rsp+1*8] 
      89	  mov [rsp], rdx ; access a var "temp5"
      90	  sub rsp, 8 ; allocate for var "temp6"
      91		; expected_arity = 1
      92		; formal_arity = 1
      93		; calling "fst"
      94	  ; expected_arity = formal_arity = 1
      95	  sub rsp, 8 ; allocate for argument 0 (name = __temp7)
      96	  mov rdx, [rsp+2*8] 
      97	  mov [rsp], rdx ; access a var "store"
      98	  call fst
      99	  add rsp, 8 ; deallocate var "__temp7"
     100	  mov [rsp], rax
     101	  sub rsp, 8 ; allocate for var "n3"
     102	  mov rdx, [rsp+1*8] 
     103	  mov [rsp], rdx ; access a var "temp6"
     104	  sub rsp, 8 ; allocate for var "temp7"
     105		; expected_arity = 1
     106		; formal_arity = 1
     107		; calling "snd"
     108	  ; expected_arity = formal_arity = 1
     109	  sub rsp, 8 ; allocate for argument 0 (name = __temp8)
     110	  mov rdx, [rsp+4*8] 
     111	  mov [rsp], rdx ; access a var "store"
     112	  call snd
     113	  add rsp, 8 ; deallocate var "__temp8"
     114	  mov [rsp], rax
     115	  sub rsp, 8 ; allocate for var "temp8"
     116		; expected_arity = 1
     117		; formal_arity = 1
     118		; calling "fst"
     119	  ; expected_arity = formal_arity = 1
     120	  sub rsp, 8 ; allocate for argument 0 (name = __temp9)
     121	  mov rdx, [rsp+2*8] 
     122	  mov [rsp], rdx ; access a var "temp7"
     123	  call fst
     124	  add rsp, 8 ; deallocate var "__temp9"
     125	  mov [rsp], rax
     126	  sub rsp, 8 ; allocate for var "n2"
     127	  mov rdx, [rsp+1*8] 
     128	  mov [rsp], rdx ; access a var "temp8"
     129	  sub rsp, 8 ; allocate for var "temp9"
     130		; expected_arity = 1
     131		; formal_arity = 1
     132		; calling "snd"
     133	  ; expected_arity = formal_arity = 1
     134	  sub rsp, 8 ; allocate for argument 0 (name = __temp10)
     135	  mov rdx, [rsp+7*8] 
     136	  mov [rsp], rdx ; access a var "store"
     137	  call snd
     138	  add rsp, 8 ; deallocate var "__temp10"
     139	  mov [rsp], rax
     140	  sub rsp, 8 ; allocate for var "temp10"
     141		; expected_arity = 1
     142		; formal_arity = 1
     143		; calling "snd"
     144	  ; expected_arity = formal_arity = 1
     145	  sub rsp, 8 ; allocate for argument 0 (name = __temp11)
     146	  mov rdx, [rsp+2*8] 
     147	  mov [rsp], rdx ; access a var "temp9"
     148	  call snd
     149	  add rsp, 8 ; deallocate var "__temp11"
     150	  mov [rsp], rax
     151	  sub rsp, 8 ; allocate for var "temp11"
     152		; expected_arity = 1
     153		; formal_arity = 1
     154		; calling "fst"
     155	  ; expected_arity = formal_arity = 1
     156	  sub rsp, 8 ; allocate for argument 0 (name = __temp12)
     157	  mov rdx, [rsp+2*8] 
     158	  mov [rsp], rdx ; access a var "temp10"
     159	  call fst
     160	  add rsp, 8 ; deallocate var "__temp12"
     161	  mov [rsp], rax
     162	  sub rsp, 8 ; allocate for var "n1"
     163	  mov rdx, [rsp+1*8] 
     164	  mov [rsp], rdx ; access a var "temp11"
     165	  sub rsp, 8 ; allocate for var "temp12"
     166	  sub rsp, 8 ; allocate for var "__temp13"
     167	  mov rdx, [rsp+9*8] 
     168	  mov [rsp], rdx ; access a var "n3"
     169	  sub rsp, 8 ; allocate for var "__temp14"
     170	  mov rdx, [rsp+7*8] 
     171	  mov [rsp], rdx ; access a var "n2"
     172	  mov rax, [8*1+rsp]
     173	  mov rbx, [rsp]
     174	  imul rbx, rax
     175	  mov [8*2+rsp], rbx
     176	  add rsp, 8 ; deallocate var "__temp14"
     177	  add rsp, 8 ; deallocate var "__temp13"
     178	  sub rsp, 8 ; allocate for var "__temp15"
     179	  mov rdx, [rsp+1*8] 
     180	  mov [rsp], rdx ; access a var "temp12"
     181	  sub rsp, 8 ; allocate for var "__temp16"
     182	  mov rdx, [rsp+3*8] 
     183	  mov [rsp], rdx ; access a var "n1"
     184	  mov rax, [8*1+rsp]
     185	  mov rbx, [rsp]
     186	  imul rbx, rax
     187	  mov rax, rbx
     188	  add rsp, 8 ; deallocate var "__temp16"
     189	  add rsp, 8 ; deallocate var "__temp15"
     190	  add rsp, 8 ; deallocate var "temp12"
     191	  add rsp, 8 ; deallocate var "n1"
     192	  add rsp, 8 ; deallocate var "temp11"
     193	  add rsp, 8 ; deallocate var "temp10"
     194	  add rsp, 8 ; deallocate var "temp9"
     195	  add rsp, 8 ; deallocate var "n2"
     196	  add rsp, 8 ; deallocate var "temp8"
     197	  add rsp, 8 ; deallocate var "temp7"
     198	  add rsp, 8 ; deallocate var "n3"
     199	  add rsp, 8 ; deallocate var "temp6"
     200	  add rsp, 8 ; deallocate var "store"
     201	  add rsp, 8 ; deallocate var "temp5"
     202	  add rsp, 8 ; deallocate var "temp4"
     203	  add rsp, 8 ; deallocate var "temp3"
     204	  pop rbp
     205	  ret  ;;;; fac3
     206	
     207		; @[{stack||stack}@]
     208	GLOBAL main
     209	main:
     210	  push rbp
     211	  mov  rbp, rsp
     212	mov rdi, rsp
     213	call rukaml_initialize
     214	  sub rsp, 8 ; allocate for var "temp14"
     215	  mov qword rdi,  1
     216	mov qword rsi, 0
     217	  call rukaml_alloc_pair
     218	  mov [rsp], rax
     219	  sub rsp, 8 ; allocate for var "temp15"
     220	  mov qword rdi,  2
     221	  mov rdx, [rsp+1*8] 
     222	  mov rsi, rdx ; access a var "temp14"
     223	  call rukaml_alloc_pair
     224	  mov [rsp], rax
     225	  sub rsp, 8 ; allocate for var "temp16"
     226	  mov qword rdi,  3
     227	  mov rdx, [rsp+1*8] 
     228	  mov rsi, rdx ; access a var "temp15"
     229	  call rukaml_alloc_pair
     230	  mov [rsp], rax
     231	  sub rsp, 8 ; allocate for var "alive"
     232	  mov rdx, [rsp+1*8] 
     233	  mov [rsp], rdx ; access a var "temp16"
     234	  sub rsp, 8 ; allocate for var "temp17"
     235		; expected_arity = 1
     236		; formal_arity = 1
     237		; calling "fac3"
     238	  ; expected_arity = formal_arity = 1
     239	  sub rsp, 8 ; allocate for argument 0 (name = __temp19)
     240	  mov qword [rsp],  0
     241	  call fac3
     242	  add rsp, 8 ; deallocate var "__temp19"
     243	  mov [rsp], rax
     244	  sub rsp, 8 ; allocate for var "tmp1"
     245	  mov rdx, [rsp+1*8] 
     246	  mov [rsp], rdx ; access a var "temp17"
     247	  sub rsp, 8 ; allocate for var "temp18"
     248	  mov rdi, rsp
     249	  mov rsi, 0
     250	  call rukaml_gc_compact
     251	  sub rsp, 8 ; allocate for var "tmp2"
     252	  mov rdx, [rsp+1*8] 
     253	  mov [rsp], rdx ; access a var "temp18"
     254	  sub rsp, 8 ; allocate for var "temp19"
     255	  mov rdi, 0
     256	  mov rsi, 0
     257	  call rukaml_gc_print_stats
     258	  sub rsp, 8 ; allocate for var "tmp4"
     259	  mov rdx, [rsp+1*8] 
     260	  mov [rsp], rdx ; access a var "temp19"
     261	  sub rsp, 8 ; allocate for var "temp20"
     262	  sub rsp, 8 ; allocate for var "__temp20"
     263	  mov qword [rsp],  42
     264	  mov rdi, [rsp]
     265	  call rukaml_print_int
     266	  mov [8*1+rsp], rax
     267	  add rsp, 8 ; deallocate var "__temp20"
     268	  sub rsp, 8 ; allocate for var "tmpl"
     269	  mov rdx, [rsp+1*8] 
     270	  mov [rsp], rdx ; access a var "temp20"
     271	  mov qword rax,  0
     272	  add rsp, 8 ; deallocate var "tmpl"
     273	  add rsp, 8 ; deallocate var "temp20"
     274	  add rsp, 8 ; deallocate var "tmp4"
     275	  add rsp, 8 ; deallocate var "temp19"
     276	  add rsp, 8 ; deallocate var "tmp2"
     277	  add rsp, 8 ; deallocate var "temp18"
     278	  add rsp, 8 ; deallocate var "tmp1"
     279	  add rsp, 8 ; deallocate var "temp17"
     280	  add rsp, 8 ; deallocate var "alive"
     281	  add rsp, 8 ; deallocate var "temp16"
     282	  add rsp, 8 ; deallocate var "temp15"
     283	  add rsp, 8 ; deallocate var "temp14"
     284	  pop rbp
     285	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc program.o ../../back_amd64/rukaml_stdlib.o -o program.exe 
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
$ ulimit -s 64
$ export RUKAMLRUNPARAM=v=2048,m=128
  $ chmod u+x program.exe && ./program.exe
  GC statistics
  Total allocations: 18(words)
  Currently allocated: 9(words)
  Current bank: 0
  42
