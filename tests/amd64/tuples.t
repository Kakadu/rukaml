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
      11	GLOBAL fst
      12	
      13	fst:
      14	  push rbp
      15	  mov  rbp, rsp
      16	  sub rsp, 8 ; allocate for var "temp1"
      17	  mov rdx, [rsp+3*8] 
      18	  mov [rsp], rdx ; access a var "p"
      19	  sub rsp, 8 ; allocate for var "a"
      20	  mov rdx, [rsp+1*8] 
      21	  mov rsi, rdx ; access a var "temp1"
      22	  mov rdi, 0
      23	  call rukaml_field
      24	  mov [rsp], rax
      25	  sub rsp, 8 ; allocate for var "b"
      26	  mov rdx, [rsp+2*8] 
      27	  mov rsi, rdx ; access a var "temp1"
      28	  mov rdi, 1
      29	  call rukaml_field
      30	  mov [rsp], rax
      31	  mov rdx, [rsp+1*8] 
      32	  mov rax, rdx ; access a var "a"
      33	  add rsp, 8 ; deallocate var "b"
      34	  add rsp, 8 ; deallocate var "a"
      35	  add rsp, 8 ; deallocate var "temp1"
      36	  pop rbp
      37	  ret  ;;;; fst
      38	GLOBAL snd
      39	snd:
      40	  push rbp
      41	  mov  rbp, rsp
      42	  sub rsp, 8 ; allocate for var "temp2"
      43	  mov rdx, [rsp+3*8] 
      44	  mov [rsp], rdx ; access a var "p"
      45	  sub rsp, 8 ; allocate for var "a"
      46	  mov rdx, [rsp+1*8] 
      47	  mov rsi, rdx ; access a var "temp2"
      48	  mov rdi, 0
      49	  call rukaml_field
      50	  mov [rsp], rax
      51	  mov rdx, [rsp+1*8] 
      52	  mov rsi, rdx ; access a var "temp2"
      53	  mov rdi, 1
      54	  call rukaml_field
      55	  mov rax, rax
      56	  add rsp, 8 ; deallocate var "a"
      57	  add rsp, 8 ; deallocate var "temp2"
      58	  pop rbp
      59	  ret  ;;;; snd
      60	GLOBAL fac3
      61	fac3:
      62	  push rbp
      63	  mov  rbp, rsp
      64	  sub rsp, 8 ; allocate for var "temp3"
      65	  mov qword rdi,  1
      66	mov qword rsi, 0
      67	  call rukaml_alloc_pair
      68	  mov [rsp], rax
      69	  sub rsp, 8 ; allocate for var "temp4"
      70	  mov qword rdi,  2
      71	  mov rdx, [rsp+1*8] 
      72	  mov rsi, rdx ; access a var "temp3"
      73	  call rukaml_alloc_pair
      74	  mov [rsp], rax
      75	  sub rsp, 8 ; allocate for var "temp5"
      76	  mov qword rdi,  3
      77	  mov rdx, [rsp+1*8] 
      78	  mov rsi, rdx ; access a var "temp4"
      79	  call rukaml_alloc_pair
      80	  mov [rsp], rax
      81	  sub rsp, 8 ; allocate for var "store"
      82	  mov rdx, [rsp+1*8] 
      83	  mov [rsp], rdx ; access a var "temp5"
      84	  sub rsp, 8 ; allocate for var "temp6"
      85		; expected_arity = 1
      86		; formal_arity = 1
      87		; calling "fst"
      88	  ; expected_arity = formal_arity = 1
      89	  sub rsp, 8 ; allocate for argument 0 (name = __temp7)
      90	  mov rdx, [rsp+2*8] 
      91	  mov [rsp], rdx ; access a var "store"
      92	  call fst
      93	  add rsp, 8 ; deallocate var "__temp7"
      94	  mov [rsp], rax
      95	  sub rsp, 8 ; allocate for var "n3"
      96	  mov rdx, [rsp+1*8] 
      97	  mov [rsp], rdx ; access a var "temp6"
      98	  sub rsp, 8 ; allocate for var "temp7"
      99		; expected_arity = 1
     100		; formal_arity = 1
     101		; calling "snd"
     102	  ; expected_arity = formal_arity = 1
     103	  sub rsp, 8 ; allocate for argument 0 (name = __temp8)
     104	  mov rdx, [rsp+4*8] 
     105	  mov [rsp], rdx ; access a var "store"
     106	  call snd
     107	  add rsp, 8 ; deallocate var "__temp8"
     108	  mov [rsp], rax
     109	  sub rsp, 8 ; allocate for var "temp8"
     110		; expected_arity = 1
     111		; formal_arity = 1
     112		; calling "fst"
     113	  ; expected_arity = formal_arity = 1
     114	  sub rsp, 8 ; allocate for argument 0 (name = __temp9)
     115	  mov rdx, [rsp+2*8] 
     116	  mov [rsp], rdx ; access a var "temp7"
     117	  call fst
     118	  add rsp, 8 ; deallocate var "__temp9"
     119	  mov [rsp], rax
     120	  sub rsp, 8 ; allocate for var "n2"
     121	  mov rdx, [rsp+1*8] 
     122	  mov [rsp], rdx ; access a var "temp8"
     123	  sub rsp, 8 ; allocate for var "temp9"
     124		; expected_arity = 1
     125		; formal_arity = 1
     126		; calling "snd"
     127	  ; expected_arity = formal_arity = 1
     128	  sub rsp, 8 ; allocate for argument 0 (name = __temp10)
     129	  mov rdx, [rsp+7*8] 
     130	  mov [rsp], rdx ; access a var "store"
     131	  call snd
     132	  add rsp, 8 ; deallocate var "__temp10"
     133	  mov [rsp], rax
     134	  sub rsp, 8 ; allocate for var "temp10"
     135		; expected_arity = 1
     136		; formal_arity = 1
     137		; calling "snd"
     138	  ; expected_arity = formal_arity = 1
     139	  sub rsp, 8 ; allocate for argument 0 (name = __temp11)
     140	  mov rdx, [rsp+2*8] 
     141	  mov [rsp], rdx ; access a var "temp9"
     142	  call snd
     143	  add rsp, 8 ; deallocate var "__temp11"
     144	  mov [rsp], rax
     145	  sub rsp, 8 ; allocate for var "temp11"
     146		; expected_arity = 1
     147		; formal_arity = 1
     148		; calling "fst"
     149	  ; expected_arity = formal_arity = 1
     150	  sub rsp, 8 ; allocate for argument 0 (name = __temp12)
     151	  mov rdx, [rsp+2*8] 
     152	  mov [rsp], rdx ; access a var "temp10"
     153	  call fst
     154	  add rsp, 8 ; deallocate var "__temp12"
     155	  mov [rsp], rax
     156	  sub rsp, 8 ; allocate for var "n1"
     157	  mov rdx, [rsp+1*8] 
     158	  mov [rsp], rdx ; access a var "temp11"
     159	  sub rsp, 8 ; allocate for var "temp12"
     160	  sub rsp, 8 ; allocate for var "__temp13"
     161	  mov rdx, [rsp+9*8] 
     162	  mov [rsp], rdx ; access a var "n3"
     163	  sub rsp, 8 ; allocate for var "__temp14"
     164	  mov rdx, [rsp+7*8] 
     165	  mov [rsp], rdx ; access a var "n2"
     166	  mov rax, [8*1+rsp]
     167	  mov rbx, [rsp]
     168	  imul rbx, rax
     169	  mov [8*2+rsp], rbx
     170	  add rsp, 8 ; deallocate var "__temp14"
     171	  add rsp, 8 ; deallocate var "__temp13"
     172	  sub rsp, 8 ; allocate for var "__temp15"
     173	  mov rdx, [rsp+1*8] 
     174	  mov [rsp], rdx ; access a var "temp12"
     175	  sub rsp, 8 ; allocate for var "__temp16"
     176	  mov rdx, [rsp+3*8] 
     177	  mov [rsp], rdx ; access a var "n1"
     178	  mov rax, [8*1+rsp]
     179	  mov rbx, [rsp]
     180	  imul rbx, rax
     181	  mov rax, rbx
     182	  add rsp, 8 ; deallocate var "__temp16"
     183	  add rsp, 8 ; deallocate var "__temp15"
     184	  add rsp, 8 ; deallocate var "temp12"
     185	  add rsp, 8 ; deallocate var "n1"
     186	  add rsp, 8 ; deallocate var "temp11"
     187	  add rsp, 8 ; deallocate var "temp10"
     188	  add rsp, 8 ; deallocate var "temp9"
     189	  add rsp, 8 ; deallocate var "n2"
     190	  add rsp, 8 ; deallocate var "temp8"
     191	  add rsp, 8 ; deallocate var "temp7"
     192	  add rsp, 8 ; deallocate var "n3"
     193	  add rsp, 8 ; deallocate var "temp6"
     194	  add rsp, 8 ; deallocate var "store"
     195	  add rsp, 8 ; deallocate var "temp5"
     196	  add rsp, 8 ; deallocate var "temp4"
     197	  add rsp, 8 ; deallocate var "temp3"
     198	  pop rbp
     199	  ret  ;;;; fac3
     200	GLOBAL main
     201	main:
     202	  push rbp
     203	  mov  rbp, rsp
     204	  mov rdi, rsp
     205	  call rukaml_initialize
     206	  sub rsp, 8 ; allocate for var "temp14"
     207	  mov qword rdi,  1
     208	mov qword rsi, 0
     209	  call rukaml_alloc_pair
     210	  mov [rsp], rax
     211	  sub rsp, 8 ; allocate for var "temp15"
     212	  mov qword rdi,  2
     213	  mov rdx, [rsp+1*8] 
     214	  mov rsi, rdx ; access a var "temp14"
     215	  call rukaml_alloc_pair
     216	  mov [rsp], rax
     217	  sub rsp, 8 ; allocate for var "temp16"
     218	  mov qword rdi,  3
     219	  mov rdx, [rsp+1*8] 
     220	  mov rsi, rdx ; access a var "temp15"
     221	  call rukaml_alloc_pair
     222	  mov [rsp], rax
     223	  sub rsp, 8 ; allocate for var "alive"
     224	  mov rdx, [rsp+1*8] 
     225	  mov [rsp], rdx ; access a var "temp16"
     226	  sub rsp, 8 ; allocate for var "temp17"
     227		; expected_arity = 1
     228		; formal_arity = 1
     229		; calling "fac3"
     230	  ; expected_arity = formal_arity = 1
     231	  sub rsp, 8 ; allocate for argument 0 (name = __temp19)
     232	  mov qword [rsp],  0
     233	  call fac3
     234	  add rsp, 8 ; deallocate var "__temp19"
     235	  mov [rsp], rax
     236	  sub rsp, 8 ; allocate for var "tmp1"
     237	  mov rdx, [rsp+1*8] 
     238	  mov [rsp], rdx ; access a var "temp17"
     239	  sub rsp, 8 ; allocate for var "temp18"
     240	  mov rdi, rsp
     241	  mov rsi, 0
     242	  call rukaml_gc_compact
     243	  sub rsp, 8 ; allocate for var "tmp2"
     244	  mov rdx, [rsp+1*8] 
     245	  mov [rsp], rdx ; access a var "temp18"
     246	  sub rsp, 8 ; allocate for var "temp19"
     247	  mov rdi, 0
     248	  mov rsi, 0
     249	  call rukaml_gc_print_stats
     250	  sub rsp, 8 ; allocate for var "tmp4"
     251	  mov rdx, [rsp+1*8] 
     252	  mov [rsp], rdx ; access a var "temp19"
     253	  sub rsp, 8 ; allocate for var "temp20"
     254	  sub rsp, 8 ; allocate for var "__temp20"
     255	  mov qword [rsp],  42
     256	  mov rdi, [rsp]
     257	  call rukaml_print_int
     258	  mov [8*1+rsp], rax
     259	  add rsp, 8 ; deallocate var "__temp20"
     260	  sub rsp, 8 ; allocate for var "tmpl"
     261	  mov rdx, [rsp+1*8] 
     262	  mov [rsp], rdx ; access a var "temp20"
     263	  mov qword rax,  0
     264	  add rsp, 8 ; deallocate var "tmpl"
     265	  add rsp, 8 ; deallocate var "temp20"
     266	  add rsp, 8 ; deallocate var "tmp4"
     267	  add rsp, 8 ; deallocate var "temp19"
     268	  add rsp, 8 ; deallocate var "tmp2"
     269	  add rsp, 8 ; deallocate var "temp18"
     270	  add rsp, 8 ; deallocate var "tmp1"
     271	  add rsp, 8 ; deallocate var "temp17"
     272	  add rsp, 8 ; deallocate var "alive"
     273	  add rsp, 8 ; deallocate var "temp16"
     274	  add rsp, 8 ; deallocate var "temp15"
     275	  add rsp, 8 ; deallocate var "temp14"
     276	  pop rbp
     277	  ret  ;;;; main
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
