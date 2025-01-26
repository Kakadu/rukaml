  $ cat > main.ml << EOF
  > let rec fack n k = if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > let id u = u
  > let main =
  >   let rez = fack 5 id in
  >   let t = print rez in
  >   0
  > EOF
$ cat main.ml
$ ls
  $ ../../back_amd64/amd64_compiler.exe -o program.asm -c main.ml # -vamd64
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
      11	
      12	GLOBAL fresh_1
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
      37	
      38	GLOBAL fack
      39	fack:
      40	  push rbp
      41	  mov  rbp, rsp
      42	  sub rsp, 8*5 ; allocate for local variables temp8, temp7, temp6, temp5, temp3
      43	  sub rsp, 8 ; allocate padding for locals
      44	  mov qword r11, [rbp+2*8]
      45	  mov qword r12, 1
      46	  cmp r11, r12
      47	  je lab_16
      48	  mov qword [rbp-1*8], 0
      49	  jmp lab_17
      50	lab_16:
      51	  mov qword [rbp-1*8], 1
      52	  jmp lab_17
      53	lab_17:
      54	  mov qword rdx, [rbp-1*8]
      55	  cmp rdx, 0
      56	  je lab_then_18
      57	  sub rsp, 8 ; padding
      58	  sub rsp, 8 ; first arg of a function k
      59	  mov qword [rbp-8*8],  1
      60	  mov rax, 0  ; no float arguments
      61	  mov rdi, [rbp+3*8]
      62	  mov rsi, 1
      63	  mov rdx, [rbp-8*8]
      64	  call rukaml_applyN
      65	  add rsp, 8*2 ; free space for args of function "k"
      66	  mov rax, rax
      67	  jmp lab_endif_19
      68	lab_then_18:
      69	  mov qword r11, [rbp+2*8]
      70	  dec r11
      71	  mov qword [rbp-2*8], r11
      72	  mov rdi, fack
      73	  mov rsi, 2
      74	  call rukaml_alloc_closure
      75	  sub rsp, 8 ; trying to save alignment 16 bytes
      76	  sub rsp, 8*1 ; fun arguments
      77	  mov qword r8, [rbp-2*8]  ; arg "temp5"
      78	  mov qword [rsp+0*8], r8
      79	  mov rdi, rax
      80	  mov rsi, 1
      81	  mov rdx, [rsp+8*0]
      82	  mov al, 0
      83	  call rukaml_applyN
      84	  add rsp, 8*2 ; deallocate args of rukaml_applyN
      85	  mov [rbp-3*8], rax
      86	  mov rdi, fresh_1
      87	  mov rsi, 3
      88	  call rukaml_alloc_closure
      89	  sub rsp, 8 ; trying to save alignment 16 bytes
      90	  sub rsp, 8*1 ; fun arguments
      91	  mov qword r8, [rbp+2*8]  ; arg "n"
      92	  mov qword [rsp+0*8], r8
      93	  mov rdi, rax
      94	  mov rsi, 1
      95	  mov rdx, [rsp+8*0]
      96	  mov al, 0
      97	  call rukaml_applyN
      98	  add rsp, 8*2 ; deallocate args of rukaml_applyN
      99	  mov [rbp-4*8], rax
     100	  sub rsp, 8 ; padding
     101	  sub rsp, 8 ; first arg of a function temp7
     102	  mov qword rdx, [rbp+3*8] ; use temp rdx to move from stack to stack
     103	  mov qword [rbp-8*8], rdx ; access a var "k"
     104	  mov rax, 0  ; no float arguments
     105	  mov rdi, [rbp-4*8]
     106	  mov rsi, 1
     107	  mov rdx, [rbp-8*8]
     108	  call rukaml_applyN
     109	  add rsp, 8*2 ; free space for args of function "temp7"
     110	  mov [rbp-5*8], rax
     111	  sub rsp, 8 ; padding
     112	  sub rsp, 8 ; first arg of a function temp6
     113	  mov qword rdx, [rbp-5*8] ; use temp rdx to move from stack to stack
     114	  mov qword [rbp-8*8], rdx ; access a var "temp8"
     115	  mov rax, 0  ; no float arguments
     116	  mov rdi, [rbp-3*8]
     117	  mov rsi, 1
     118	  mov rdx, [rbp-8*8]
     119	  call rukaml_applyN
     120	  add rsp, 8*2 ; free space for args of function "temp6"
     121	  mov rax, rax
     122	lab_endif_19:
     123	  add rsp, 8 ; deallocate padding for locals
     124	  add rsp, 8*5 ; deallocate local variables temp8, temp7, temp6, temp5, temp3
     125	  pop rbp
     126	  ret  ;;;; fack
     127	
     128	GLOBAL id
     129	id:
     130	  push rbp
     131	  mov  rbp, rsp
     132	  mov qword rdx, [rbp+2*8] ; use temp rdx to move from stack to stack
     133	  mov qword rax, rdx ; access a var "u"
     134	  pop rbp
     135	  ret  ;;;; id
     136	
     137	GLOBAL main
     138	main:
     139	  push rbp
     140	  mov  rbp, rsp
     141	  mov rdi, rsp
     142	  call rukaml_initialize
     143	  sub rsp, 8*3 ; allocate for local variables t, rez, temp11
     144	  sub rsp, 8 ; allocate padding for locals
     145	  mov rdi, fack
     146	  mov rsi, 2
     147	  call rukaml_alloc_closure
     148	  sub rsp, 8 ; trying to save alignment 16 bytes
     149	  sub rsp, 8*1 ; fun arguments
     150	  mov qword [rsp+0*8], 5 ; constant
     151	  mov rdi, rax
     152	  mov rsi, 1
     153	  mov rdx, [rsp+8*0]
     154	  mov al, 0
     155	  call rukaml_applyN
     156	  add rsp, 8*2 ; deallocate args of rukaml_applyN
     157	  mov [rbp-1*8], rax
     158	  sub rsp, 8 ; padding
     159	  sub rsp, 8 ; first arg of a function temp11
     160	  mov rdi, id
     161	  mov rsi, 1
     162	  call rukaml_alloc_closure
     163	  mov [rbp-6*8], rax
     164	  mov rax, 0  ; no float arguments
     165	  mov rdi, [rbp-1*8]
     166	  mov rsi, 1
     167	  mov rdx, [rbp-6*8]
     168	  call rukaml_applyN
     169	  add rsp, 8*2 ; free space for args of function "temp11"
     170	  mov [rbp-2*8], rax
     171	  mov rdi, [rbp-2*8]
     172	  call rukaml_print_int ; short
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
