  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -c - #-vamd64
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
      11	
      12	GLOBAL fst
      13	fst:
      14	  push rbp
      15	  mov  rbp, rsp
      16	  sub rsp, 8*3 ; allocate for local variables b, a, temp1
      17	  sub rsp, 8 ; allocate padding for locals
      18	  mov qword rdx, [rbp+2*8] ; use temp rdx to move from stack to stack
      19	  mov qword [rbp-1*8], rdx ; access a var "p"
      20	  mov qword rdx, [rbp-1*8] ; use temp rdx to move from stack to stack
      21	  mov qword rsi, rdx ; access a var "temp1"
      22	  mov rdi, 0
      23	  call rukaml_field
      24	  mov [rbp-2*8], rax
      25	  mov qword rdx, [rbp-1*8] ; use temp rdx to move from stack to stack
      26	  mov qword rsi, rdx ; access a var "temp1"
      27	  mov rdi, 1
      28	  call rukaml_field
      29	  mov [rbp-3*8], rax
      30	  mov qword rdx, [rbp-2*8] ; use temp rdx to move from stack to stack
      31	  mov qword rax, rdx ; access a var "a"
      32	  add rsp, 8 ; deallocate padding for locals
      33	  add rsp, 8*3 ; deallocate local variables b, a, temp1
      34	  pop rbp
      35	  ret  ;;;; fst
      36	
      37	GLOBAL snd
      38	snd:
      39	  push rbp
      40	  mov  rbp, rsp
      41	  sub rsp, 8*2 ; allocate for local variables a, temp2
      42	  mov qword rdx, [rbp+2*8] ; use temp rdx to move from stack to stack
      43	  mov qword [rbp-1*8], rdx ; access a var "p"
      44	  mov qword rdx, [rbp-1*8] ; use temp rdx to move from stack to stack
      45	  mov qword rsi, rdx ; access a var "temp2"
      46	  mov rdi, 0
      47	  call rukaml_field
      48	  mov [rbp-2*8], rax
      49	  mov qword rdx, [rbp-1*8] ; use temp rdx to move from stack to stack
      50	  mov qword rsi, rdx ; access a var "temp2"
      51	  mov rdi, 1
      52	  call rukaml_field
      53	  mov rax, rax
      54	  add rsp, 8*2 ; deallocate local variables a, temp2
      55	  pop rbp
      56	  ret  ;;;; snd
      57	
      58	GLOBAL fac3
      59	fac3:
      60	  push rbp
      61	  mov  rbp, rsp
      62	  sub rsp, 8*10 ; allocate for local variables temp12, n1, temp10, temp9, n2, temp7, n3, store, temp4, temp3
      63	  mov qword rdi,  1
      64	mov qword rsi, 0
      65	  call rukaml_alloc_pair
      66	  mov [rbp-1*8], rax
      67	  mov qword rdi,  2
      68	  mov qword rdx, [rbp-1*8] ; use temp rdx to move from stack to stack
      69	  mov qword rsi, rdx ; access a var "temp3"
      70	  call rukaml_alloc_pair
      71	  mov [rbp-2*8], rax
      72	  mov qword rdi,  3
      73	  mov qword rdx, [rbp-2*8] ; use temp rdx to move from stack to stack
      74	  mov qword rsi, rdx ; access a var "temp4"
      75	  call rukaml_alloc_pair
      76	  mov [rbp-3*8], rax
      77	  sub rsp, 8 ; trying to save alignment 16 bytes
      78	  sub rsp, 8*1 ; fun arguments
      79	  mov qword r8, [rbp-3*8]  ; arg "store"
      80	  mov qword [rsp+0*8], r8
      81	  call fst
      82	  add rsp, 8*2 ; dealloc args
      83	  mov [rbp-4*8], rax
      84	  sub rsp, 8 ; trying to save alignment 16 bytes
      85	  sub rsp, 8*1 ; fun arguments
      86	  mov qword r8, [rbp-3*8]  ; arg "store"
      87	  mov qword [rsp+0*8], r8
      88	  call snd
      89	  add rsp, 8*2 ; dealloc args
      90	  mov [rbp-5*8], rax
      91	  sub rsp, 8 ; trying to save alignment 16 bytes
      92	  sub rsp, 8*1 ; fun arguments
      93	  mov qword r8, [rbp-5*8]  ; arg "temp7"
      94	  mov qword [rsp+0*8], r8
      95	  call fst
      96	  add rsp, 8*2 ; dealloc args
      97	  mov [rbp-6*8], rax
      98	  sub rsp, 8 ; trying to save alignment 16 bytes
      99	  sub rsp, 8*1 ; fun arguments
     100	  mov qword r8, [rbp-3*8]  ; arg "store"
     101	  mov qword [rsp+0*8], r8
     102	  call snd
     103	  add rsp, 8*2 ; dealloc args
     104	  mov [rbp-7*8], rax
     105	  sub rsp, 8 ; trying to save alignment 16 bytes
     106	  sub rsp, 8*1 ; fun arguments
     107	  mov qword r8, [rbp-7*8]  ; arg "temp9"
     108	  mov qword [rsp+0*8], r8
     109	  call snd
     110	  add rsp, 8*2 ; dealloc args
     111	  mov [rbp-8*8], rax
     112	  sub rsp, 8 ; trying to save alignment 16 bytes
     113	  sub rsp, 8*1 ; fun arguments
     114	  mov qword r8, [rbp-8*8]  ; arg "temp10"
     115	  mov qword [rsp+0*8], r8
     116	  call fst
     117	  add rsp, 8*2 ; dealloc args
     118	  mov [rbp-9*8], rax
     119	  mov qword r11, [rbp-4*8]
     120	  mov qword r12, [rbp-6*8]
     121	  imul r11, r12
     122	  mov [rbp-10*8], r11
     123	  mov qword r11, [rbp-10*8]
     124	  mov qword r12, [rbp-9*8]
     125	  imul r11, r12
     126	  mov rax, r11
     127	  add rsp, 8*10 ; deallocate local variables temp12, n1, temp10, temp9, n2, temp7, n3, store, temp4, temp3
     128	  pop rbp
     129	  ret  ;;;; fac3
     130	
     131	GLOBAL main
     132	main:
     133	  push rbp
     134	  mov  rbp, rsp
     135	  mov rdi, rsp
     136	  call rukaml_initialize
     137	  sub rsp, 8*7 ; allocate for local variables tmpl, tmp4, tmp2, tmp1, alive, temp15, temp14
     138	  sub rsp, 8 ; allocate padding for locals
     139	  mov qword rdi,  1
     140	mov qword rsi, 0
     141	  call rukaml_alloc_pair
     142	  mov [rbp-1*8], rax
     143	  mov qword rdi,  2
     144	  mov qword rdx, [rbp-1*8] ; use temp rdx to move from stack to stack
     145	  mov qword rsi, rdx ; access a var "temp14"
     146	  call rukaml_alloc_pair
     147	  mov [rbp-2*8], rax
     148	  mov qword rdi,  3
     149	  mov qword rdx, [rbp-2*8] ; use temp rdx to move from stack to stack
     150	  mov qword rsi, rdx ; access a var "temp15"
     151	  call rukaml_alloc_pair
     152	  mov [rbp-3*8], rax
     153	  sub rsp, 8 ; trying to save alignment 16 bytes
     154	  sub rsp, 8*1 ; fun arguments
     155	  mov qword [rsp+0*8], 0 ; constant
     156	  call fac3
     157	  add rsp, 8*2 ; dealloc args
     158	  mov [rbp-4*8], rax
     159	  mov rdi, rsp
     160	  mov rsi, 0
     161	  call rukaml_gc_compact
     162	  mov rdi, 0
     163	  mov rsi, 0
     164	  call rukaml_gc_print_stats
     165	  mov rdi, 42
     166	  call rukaml_print_int
     167	  mov [rbp-7*8], rax
     168	  mov qword rax,  0
     169	  add rsp, 8 ; deallocate padding for locals
     170	  add rsp, 8*7 ; deallocate local variables tmpl, tmp4, tmp2, tmp1, alive, temp15, temp14
     171	  pop rbp
     172	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe
$ ulimit -s 64
$ export RUKAMLRUNPARAM=v=2048,m=128
  $ chmod u+x program.exe && ./program.exe
  GC statistics
  Total allocations: 18(words)
  Currently allocated: 9(words)
  Current bank: 0
  rukaml_print_int 42
