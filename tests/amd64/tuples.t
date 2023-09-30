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
        let store = (3, temp4) in
          let n3 = fst store  in
            let temp7 = snd store  in
              let n2 = fst temp7  in
                let temp9 = snd store  in
                  let temp10 = snd temp9  in
                    let n1 = fst temp10  in
                      let temp12 = (n3 * n2) in
                        (temp12 * n1)
  let main =
    let temp14 = (1, ()) in
      let temp15 = (2, temp14) in
        let alive = (3, temp15) in
          let tmp1 = fac3 0  in
            let tmp2 = gc_compact ()  in
              let tmp4 = gc_stats ()  in
                let tmpl = print 42  in
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
             let store = (3, temp4) in
               let n3 = fst store  in
                 let temp7 = snd store  in
                   let n2 = fst temp7  in
                     let temp9 = snd store  in
                       let temp10 = snd temp9  in
                         let n1 = fst temp10  in
                           let temp12 = (n3 * n2) in
                             (temp12 * n1)
       let main =
         let temp14 = (1, ()) in
           let temp15 = (2, temp14) in
             let alive = (3, temp15) in
               let tmp1 = fac3 0  in
                 let tmp2 = gc_compact ()  in
                   let tmp4 = gc_stats ()  in
                     let tmpl = print 42  in
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
      75	  sub rsp, 8 ; allocate for var "store"
      76	  mov qword rdi,  3
      77	  mov rdx, [rsp+1*8] 
      78	  mov rsi, rdx ; access a var "temp4"
      79	  call rukaml_alloc_pair
      80	  mov [rsp], rax
      81	  sub rsp, 8 ; allocate for var "n3"
      82		; expected_arity = 1
      83		; formal_arity = 1
      84		; calling "fst"
      85	  ; expected_arity = formal_arity = 1
      86	  sub rsp, 8 ; allocate for argument 0 (name = __temp7)
      87	  mov rdx, [rsp+2*8] 
      88	  mov [rsp], rdx ; access a var "store"
      89	  call fst
      90	  add rsp, 8 ; deallocate var "__temp7"
      91	  mov [rsp], rax
      92	  sub rsp, 8 ; allocate for var "temp7"
      93		; expected_arity = 1
      94		; formal_arity = 1
      95		; calling "snd"
      96	  ; expected_arity = formal_arity = 1
      97	  sub rsp, 8 ; allocate for argument 0 (name = __temp8)
      98	  mov rdx, [rsp+3*8] 
      99	  mov [rsp], rdx ; access a var "store"
     100	  call snd
     101	  add rsp, 8 ; deallocate var "__temp8"
     102	  mov [rsp], rax
     103	  sub rsp, 8 ; allocate for var "n2"
     104		; expected_arity = 1
     105		; formal_arity = 1
     106		; calling "fst"
     107	  ; expected_arity = formal_arity = 1
     108	  sub rsp, 8 ; allocate for argument 0 (name = __temp9)
     109	  mov rdx, [rsp+2*8] 
     110	  mov [rsp], rdx ; access a var "temp7"
     111	  call fst
     112	  add rsp, 8 ; deallocate var "__temp9"
     113	  mov [rsp], rax
     114	  sub rsp, 8 ; allocate for var "temp9"
     115		; expected_arity = 1
     116		; formal_arity = 1
     117		; calling "snd"
     118	  ; expected_arity = formal_arity = 1
     119	  sub rsp, 8 ; allocate for argument 0 (name = __temp10)
     120	  mov rdx, [rsp+5*8] 
     121	  mov [rsp], rdx ; access a var "store"
     122	  call snd
     123	  add rsp, 8 ; deallocate var "__temp10"
     124	  mov [rsp], rax
     125	  sub rsp, 8 ; allocate for var "temp10"
     126		; expected_arity = 1
     127		; formal_arity = 1
     128		; calling "snd"
     129	  ; expected_arity = formal_arity = 1
     130	  sub rsp, 8 ; allocate for argument 0 (name = __temp11)
     131	  mov rdx, [rsp+2*8] 
     132	  mov [rsp], rdx ; access a var "temp9"
     133	  call snd
     134	  add rsp, 8 ; deallocate var "__temp11"
     135	  mov [rsp], rax
     136	  sub rsp, 8 ; allocate for var "n1"
     137		; expected_arity = 1
     138		; formal_arity = 1
     139		; calling "fst"
     140	  ; expected_arity = formal_arity = 1
     141	  sub rsp, 8 ; allocate for argument 0 (name = __temp12)
     142	  mov rdx, [rsp+2*8] 
     143	  mov [rsp], rdx ; access a var "temp10"
     144	  call fst
     145	  add rsp, 8 ; deallocate var "__temp12"
     146	  mov [rsp], rax
     147	  sub rsp, 8 ; allocate for var "temp12"
     148	  sub rsp, 8 ; allocate for var "__temp13"
     149	  mov rdx, [rsp+7*8] 
     150	  mov [rsp], rdx ; access a var "n3"
     151	  sub rsp, 8 ; allocate for var "__temp14"
     152	  mov rdx, [rsp+6*8] 
     153	  mov [rsp], rdx ; access a var "n2"
     154	  mov rax, [8*1+rsp]
     155	  mov rbx, [rsp]
     156	  imul rbx, rax
     157	  mov [8*2+rsp], rbx
     158	  add rsp, 8 ; deallocate var "__temp14"
     159	  add rsp, 8 ; deallocate var "__temp13"
     160	  sub rsp, 8 ; allocate for var "__temp15"
     161	  mov rdx, [rsp+1*8] 
     162	  mov [rsp], rdx ; access a var "temp12"
     163	  sub rsp, 8 ; allocate for var "__temp16"
     164	  mov rdx, [rsp+3*8] 
     165	  mov [rsp], rdx ; access a var "n1"
     166	  mov rax, [8*1+rsp]
     167	  mov rbx, [rsp]
     168	  imul rbx, rax
     169	  mov rax, rbx
     170	  add rsp, 8 ; deallocate var "__temp16"
     171	  add rsp, 8 ; deallocate var "__temp15"
     172	  add rsp, 8 ; deallocate var "temp12"
     173	  add rsp, 8 ; deallocate var "n1"
     174	  add rsp, 8 ; deallocate var "temp10"
     175	  add rsp, 8 ; deallocate var "temp9"
     176	  add rsp, 8 ; deallocate var "n2"
     177	  add rsp, 8 ; deallocate var "temp7"
     178	  add rsp, 8 ; deallocate var "n3"
     179	  add rsp, 8 ; deallocate var "store"
     180	  add rsp, 8 ; deallocate var "temp4"
     181	  add rsp, 8 ; deallocate var "temp3"
     182	  pop rbp
     183	  ret  ;;;; fac3
     184	GLOBAL main
     185	main:
     186	  push rbp
     187	  mov  rbp, rsp
     188	  mov rdi, rsp
     189	  call rukaml_initialize
     190	  sub rsp, 8 ; allocate for var "temp14"
     191	  mov qword rdi,  1
     192	mov qword rsi, 0
     193	  call rukaml_alloc_pair
     194	  mov [rsp], rax
     195	  sub rsp, 8 ; allocate for var "temp15"
     196	  mov qword rdi,  2
     197	  mov rdx, [rsp+1*8] 
     198	  mov rsi, rdx ; access a var "temp14"
     199	  call rukaml_alloc_pair
     200	  mov [rsp], rax
     201	  sub rsp, 8 ; allocate for var "alive"
     202	  mov qword rdi,  3
     203	  mov rdx, [rsp+1*8] 
     204	  mov rsi, rdx ; access a var "temp15"
     205	  call rukaml_alloc_pair
     206	  mov [rsp], rax
     207	  sub rsp, 8 ; allocate for var "tmp1"
     208		; expected_arity = 1
     209		; formal_arity = 1
     210		; calling "fac3"
     211	  ; expected_arity = formal_arity = 1
     212	  sub rsp, 8 ; allocate for argument 0 (name = __temp19)
     213	  mov qword [rsp],  0
     214	  call fac3
     215	  add rsp, 8 ; deallocate var "__temp19"
     216	  mov [rsp], rax
     217	  sub rsp, 8 ; allocate for var "tmp2"
     218	  mov rdi, rsp
     219	  mov rsi, 0
     220	  call rukaml_gc_compact
     221	  sub rsp, 8 ; allocate for var "tmp4"
     222	  mov rdi, 0
     223	  mov rsi, 0
     224	  call rukaml_gc_print_stats
     225	  sub rsp, 8 ; allocate for var "tmpl"
     226	  sub rsp, 8 ; allocate for var "__temp20"
     227	  mov qword [rsp],  42
     228	  mov rdi, [rsp]
     229	  mov rax, 0
     230	  call rukaml_print_int
     231	  mov [8*1+rsp], rax
     232	  add rsp, 8 ; deallocate var "__temp20"
     233	  mov qword rax,  0
     234	  add rsp, 8 ; deallocate var "tmpl"
     235	  add rsp, 8 ; deallocate var "tmp4"
     236	  add rsp, 8 ; deallocate var "tmp2"
     237	  add rsp, 8 ; deallocate var "tmp1"
     238	  add rsp, 8 ; deallocate var "alive"
     239	  add rsp, 8 ; deallocate var "temp15"
     240	  add rsp, 8 ; deallocate var "temp14"
     241	  pop rbp
     242	  ret  ;;;; main
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
  rukaml_print_int 42
