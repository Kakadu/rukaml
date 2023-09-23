  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 --no-start -
  > let prod a b = a * b
  > let sum a b = a + b
  > let main =
  >   let u = print ((if 0=0 then prod else sum) 6 8) in 
  >   0
  > EOF
  After ANF transformation.
  let prod a b =
    (a * b)
  let sum a b =
    (a + b)
  let main =
    let temp3 = (0 = 0) in
      let temp4 = (if temp3
                  then prod
                  else sum) in
        let temp5 = temp4 6  in
          let temp6 = temp5 8  in
            let temp7 = print temp6  in
              let u = temp7 in
                0
  ANF: let prod a b =
         (a * b)
       let sum a b =
         (a + b)
       let main =
         let temp3 = (0 = 0) in
           let temp4 = (if temp3
                       then prod
                       else sum) in
             let temp5 = temp4 6  in
               let temp6 = temp5 8  in
                 let temp7 = print temp6  in
                   let u = temp7 in
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
      13	GLOBAL prod
      14	
      15	prod:
      16	  push rbp
      17	  mov  rbp, rsp
      18	  sub rsp, 8 ; allocate for var "__temp3"
      19	  mov rdx, [rsp+3*8] 
      20	  mov [rsp], rdx ; access a var "a"
      21	  sub rsp, 8 ; allocate for var "__temp4"
      22	  mov rdx, [rsp+5*8] 
      23	  mov [rsp], rdx ; access a var "b"
      24	  mov rax, [8*1+rsp]
      25	  mov rbx, [rsp]
      26	  imul rbx, rax
      27	  mov rax, rbx
      28	  add rsp, 8 ; deallocate var "__temp4"
      29	  add rsp, 8 ; deallocate var "__temp3"
      30	  pop rbp
      31	  ret  ;;;; prod
      32	
      33		; @[{stack||stack}@]
      34	GLOBAL sum
      35	sum:
      36	  push rbp
      37	  mov  rbp, rsp
      38	  sub rsp, 8 ; allocate for var "__temp7"
      39	  mov rdx, [rsp+3*8] 
      40	  mov [rsp], rdx ; access a var "a"
      41	  sub rsp, 8 ; allocate for var "__temp8"
      42	  mov rdx, [rsp+5*8] 
      43	  mov [rsp], rdx ; access a var "b"
      44	  mov rax, [8*1+rsp]
      45	  mov rbx, [rsp]
      46	  add  rbx, rax
      47	  mov rax, rbx
      48	  add rsp, 8 ; deallocate var "__temp8"
      49	  add rsp, 8 ; deallocate var "__temp7"
      50	  pop rbp
      51	  ret  ;;;; sum
      52	
      53		; @[{stack||stack}@]
      54	GLOBAL main
      55	main:
      56	  push rbp
      57	  mov  rbp, rsp
      58	mov rdi, rsp
      59	call rukaml_initialize
      60	  sub rsp, 8 ; allocate for var "temp3"
      61	  sub rsp, 8 ; allocate for var "__temp11"
      62	  mov qword [rsp],  0
      63	  sub rsp, 8 ; allocate for var "__temp12"
      64	  mov qword [rsp],  0
      65	  mov rax, [8*1+rsp]
      66	  mov rbx, [rsp]
      67	  cmp rax, rbx
      68	  je lab_8
      69	  mov qword [8*2+rsp], 0
      70	  jmp lab_9
      71	  lab_8:
      72	    mov qword [8*2+rsp], 1
      73	    jmp lab_9
      74	  lab_9:
      75	  add rsp, 8 ; deallocate var "__temp12"
      76	  add rsp, 8 ; deallocate var "__temp11"
      77	  sub rsp, 8 ; allocate for var "temp4"
      78	  mov rdx, [rsp+1*8] 
      79	  cmp rdx, 0
      80	  je lab_then_10
      81	  mov rdi, prod
      82	  mov rsi, 2
      83	  call rukaml_alloc_closure
      84	  mov [rsp], rax
      85	  jmp lab_endif_11
      86	  lab_then_10:
      87	  mov rdi, sum
      88	  mov rsi, 2
      89	  call rukaml_alloc_closure
      90	  mov [rsp], rax
      91	  lab_endif_11:
      92	  sub rsp, 8 ; allocate for var "temp5"
      93	  sub rsp, 8 ; allocate for var "__temp13"
      94	  mov qword [rsp],  6
      95	  mov rax, 0  ; no float arguments
      96	  mov rdi, [8*2+rsp]
      97	  mov rsi, 1
      98	  mov rdx, [rsp]
      99	  call rukaml_applyN
     100	  add rsp, 8 ; deallocate var "__temp13"
     101	  mov [rsp], rax
     102	  sub rsp, 8 ; allocate for var "temp6"
     103	  sub rsp, 8 ; allocate for var "__temp14"
     104	  mov qword [rsp],  8
     105	  mov rax, 0  ; no float arguments
     106	  mov rdi, [8*2+rsp]
     107	  mov rsi, 1
     108	  mov rdx, [rsp]
     109	  call rukaml_applyN
     110	  add rsp, 8 ; deallocate var "__temp14"
     111	  mov [rsp], rax
     112	  sub rsp, 8 ; allocate for var "temp7"
     113	  mov rdi, [8*1+rsp]
     114	  call rukaml_print_int ; short
     115	  mov [rsp], rax
     116	  sub rsp, 8 ; allocate for var "u"
     117	  mov rdx, [rsp+1*8] 
     118	  mov [rsp], rdx ; access a var "temp7"
     119	  mov qword rax,  0
     120	  add rsp, 8 ; deallocate var "u"
     121	  add rsp, 8 ; deallocate var "temp7"
     122	  add rsp, 8 ; deallocate var "temp6"
     123	  add rsp, 8 ; deallocate var "temp5"
     124	  add rsp, 8 ; deallocate var "temp4"
     125	  add rsp, 8 ; deallocate var "temp3"
     126	  pop rbp
     127	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc program.o ../../back_amd64/rukaml_stdlib.o -o program.exe 
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
  $ chmod u+x program.exe && ./program.exe
  48
