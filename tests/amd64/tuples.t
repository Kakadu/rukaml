  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm --no-start - #-vamd64
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
      11	GLOBAL fst
      12
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
      36	GLOBAL snd
      37	snd:
      38	  push rbp
      39	  mov  rbp, rsp
      40	  sub rsp, 8*2 ; allocate for local variables a, temp2
      41	  mov qword rdx, [rbp+2*8] ; use temp rdx to move from stack to stack
      42	  mov qword [rbp-1*8], rdx ; access a var "p"
      43	  mov qword rdx, [rbp-1*8] ; use temp rdx to move from stack to stack
      44	  mov qword rsi, rdx ; access a var "temp2"
      45	  mov rdi, 0
      46	  call rukaml_field
      47	  mov [rbp-2*8], rax
      48	  mov qword rdx, [rbp-1*8] ; use temp rdx to move from stack to stack
      49	  mov qword rsi, rdx ; access a var "temp2"
      50	  mov rdi, 1
      51	  call rukaml_field
      52	  mov rax, rax
      53	  add rsp, 8*2 ; deallocate local variables a, temp2
      54	  pop rbp
      55	  ret  ;;;; snd
      56	GLOBAL fac3
      57	fac3:
      58	  push rbp
      59	  mov  rbp, rsp
      60	  sub rsp, 8*10 ; allocate for local variables temp12, n1, temp10, temp9, n2, temp7, n3, store, temp4, temp3
      61	  mov qword rdi,  1
      62	mov qword rsi, 0
      63	  call rukaml_alloc_pair
      64	  mov [rbp-1*8], rax
      65	  mov qword rdi,  2
      66	  mov qword rdx, [rbp-1*8] ; use temp rdx to move from stack to stack
      67	  mov qword rsi, rdx ; access a var "temp3"
      68	  call rukaml_alloc_pair
      69	  mov [rbp-2*8], rax
      70	  mov qword rdi,  3
      71	  mov qword rdx, [rbp-2*8] ; use temp rdx to move from stack to stack
      72	  mov qword rsi, rdx ; access a var "temp4"
      73	  call rukaml_alloc_pair
      74	  mov [rbp-3*8], rax
      75	  sub rsp, 8 ; trying to save alignment 16 bytes
      76	  sub rsp, 8*1 ; fun arguments
      77	  mov qword r8, [rbp-3*8]  ; arg "store"
      78	  mov qword [rsp+0*8], r8
      79	  call fst
      80	  add rsp, 8*2 ; dealloc args
      81	  mov [rbp-4*8], rax
      82	  sub rsp, 8 ; trying to save alignment 16 bytes
      83	  sub rsp, 8*1 ; fun arguments
      84	  mov qword r8, [rbp-3*8]  ; arg "store"
      85	  mov qword [rsp+0*8], r8
      86	  call snd
      87	  add rsp, 8*2 ; dealloc args
      88	  mov [rbp-5*8], rax
      89	  sub rsp, 8 ; trying to save alignment 16 bytes
      90	  sub rsp, 8*1 ; fun arguments
      91	  mov qword r8, [rbp-5*8]  ; arg "temp7"
      92	  mov qword [rsp+0*8], r8
      93	  call fst
      94	  add rsp, 8*2 ; dealloc args
      95	  mov [rbp-6*8], rax
      96	  sub rsp, 8 ; trying to save alignment 16 bytes
      97	  sub rsp, 8*1 ; fun arguments
      98	  mov qword r8, [rbp-3*8]  ; arg "store"
      99	  mov qword [rsp+0*8], r8
     100	  call snd
     101	  add rsp, 8*2 ; dealloc args
     102	  mov [rbp-7*8], rax
     103	  sub rsp, 8 ; trying to save alignment 16 bytes
     104	  sub rsp, 8*1 ; fun arguments
     105	  mov qword r8, [rbp-7*8]  ; arg "temp9"
     106	  mov qword [rsp+0*8], r8
     107	  call snd
     108	  add rsp, 8*2 ; dealloc args
     109	  mov [rbp-8*8], rax
     110	  sub rsp, 8 ; trying to save alignment 16 bytes
     111	  sub rsp, 8*1 ; fun arguments
     112	  mov qword r8, [rbp-8*8]  ; arg "temp10"
     113	  mov qword [rsp+0*8], r8
     114	  call fst
     115	  add rsp, 8*2 ; dealloc args
     116	  mov [rbp-9*8], rax
     117	  mov qword r11, [rbp-4*8]
     118	  mov qword r12, [rbp-6*8]
     119	  imul r11, r12
     120	  mov [rbp-10*8], r11
     121	  mov qword r11, [rbp-10*8]
     122	  mov qword r12, [rbp-9*8]
     123	  imul r11, r12
     124	  mov rax, r11
     125	  add rsp, 8*10 ; deallocate local variables temp12, n1, temp10, temp9, n2, temp7, n3, store, temp4, temp3
     126	  pop rbp
     127	  ret  ;;;; fac3
     128	GLOBAL main
     129	main:
     130	  push rbp
     131	  mov  rbp, rsp
     132	  mov rdi, rsp
     133	  call rukaml_initialize
     134	  sub rsp, 8*7 ; allocate for local variables tmpl, tmp4, tmp2, tmp1, alive, temp15, temp14
     135	  sub rsp, 8 ; allocate padding for locals
     136	  mov qword rdi,  1
     137	mov qword rsi, 0
     138	  call rukaml_alloc_pair
     139	  mov [rbp-1*8], rax
     140	  mov qword rdi,  2
     141	  mov qword rdx, [rbp-1*8] ; use temp rdx to move from stack to stack
     142	  mov qword rsi, rdx ; access a var "temp14"
     143	  call rukaml_alloc_pair
     144	  mov [rbp-2*8], rax
     145	  mov qword rdi,  3
     146	  mov qword rdx, [rbp-2*8] ; use temp rdx to move from stack to stack
     147	  mov qword rsi, rdx ; access a var "temp15"
     148	  call rukaml_alloc_pair
     149	  mov [rbp-3*8], rax
     150	  sub rsp, 8 ; trying to save alignment 16 bytes
     151	  sub rsp, 8*1 ; fun arguments
     152	  mov qword [rsp+0*8], 0 ; constant
     153	  call fac3
     154	  add rsp, 8*2 ; dealloc args
     155	  mov [rbp-4*8], rax
     156	  mov rdi, rsp
     157	  mov rsi, 0
     158	  call rukaml_gc_compact
     159	  mov rdi, 0
     160	  mov rsi, 0
     161	  call rukaml_gc_print_stats
     162	  mov rdi, 42
     163	  call rukaml_print_int
     164	  mov [rbp-7*8], rax
     165	  mov qword rax,  0
     166	  add rsp, 8 ; deallocate padding for locals
     167	  add rsp, 8*7 ; deallocate local variables tmpl, tmp4, tmp2, tmp1, alive, temp15, temp14
     168	  pop rbp
     169	  ret  ;;;; main
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
