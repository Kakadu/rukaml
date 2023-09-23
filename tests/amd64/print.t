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
       5	
       6	
       7		; @[{stack||stack}@]
       8	GLOBAL prod
       9	
      10	prod:
      11	  push rbp
      12	  mov  rbp, rsp
      13	  sub rsp, 8 ; allocate for var "__temp3"
      14	  mov rdx, [rsp+3*8] 
      15	  mov [rsp], rdx ; access a var "a"
      16	  sub rsp, 8 ; allocate for var "__temp4"
      17	  mov rdx, [rsp+5*8] 
      18	  mov [rsp], rdx ; access a var "b"
      19	  mov rax, [8*1+rsp]
      20	  mov rbx, [rsp]
      21	  imul rbx, rax
      22	  mov rax, rbx
      23	  add rsp, 8 ; deallocate var "__temp4"
      24	  add rsp, 8 ; deallocate var "__temp3"
      25	  pop rbp
      26	  ret  ;;;; prod
      27	
      28		; @[{stack||stack}@]
      29	GLOBAL sum
      30	sum:
      31	  push rbp
      32	  mov  rbp, rsp
      33	  sub rsp, 8 ; allocate for var "__temp7"
      34	  mov rdx, [rsp+3*8] 
      35	  mov [rsp], rdx ; access a var "a"
      36	  sub rsp, 8 ; allocate for var "__temp8"
      37	  mov rdx, [rsp+5*8] 
      38	  mov [rsp], rdx ; access a var "b"
      39	  mov rax, [8*1+rsp]
      40	  mov rbx, [rsp]
      41	  add  rbx, rax
      42	  mov rax, rbx
      43	  add rsp, 8 ; deallocate var "__temp8"
      44	  add rsp, 8 ; deallocate var "__temp7"
      45	  pop rbp
      46	  ret  ;;;; sum
      47	
      48		; @[{stack||stack}@]
      49	GLOBAL main
      50	main:
      51	  push rbp
      52	  mov  rbp, rsp
      53	  sub rsp, 8 ; allocate for var "temp3"
      54	  sub rsp, 8 ; allocate for var "__temp11"
      55	  mov qword [rsp],  0
      56	  sub rsp, 8 ; allocate for var "__temp12"
      57	  mov qword [rsp],  0
      58	  mov rax, [8*1+rsp]
      59	  mov rbx, [rsp]
      60	  cmp rax, rbx
      61	  je lab_8
      62	  mov qword [8*2+rsp], 0
      63	  jmp lab_9
      64	  lab_8:
      65	    mov qword [8*2+rsp], 1
      66	    jmp lab_9
      67	  lab_9:
      68	  add rsp, 8 ; deallocate var "__temp12"
      69	  add rsp, 8 ; deallocate var "__temp11"
      70	  sub rsp, 8 ; allocate for var "temp4"
      71	  mov rdx, [rsp+1*8] 
      72	  cmp rdx, 0
      73	  je lab_then_10
      74	  mov rdi, prod
      75	  mov rsi, 2
      76	  call rukaml_alloc_closure
      77	  mov [rsp], rax
      78	  jmp lab_endif_11
      79	  lab_then_10:
      80	  mov rdi, sum
      81	  mov rsi, 2
      82	  call rukaml_alloc_closure
      83	  mov [rsp], rax
      84	  lab_endif_11:
      85	  sub rsp, 8 ; allocate for var "temp5"
      86	  sub rsp, 8 ; allocate for var "__temp13"
      87	  mov qword [rsp],  6
      88	  mov rax, 0  ; no float arguments
      89	  mov rdi, [8*2+rsp]
      90	  mov rsi, 1
      91	  mov rdx, [rsp]
      92	  call rukaml_applyN
      93	  add rsp, 8 ; deallocate var "__temp13"
      94	  mov [rsp], rax
      95	  sub rsp, 8 ; allocate for var "temp6"
      96	  sub rsp, 8 ; allocate for var "__temp14"
      97	  mov qword [rsp],  8
      98	  mov rax, 0  ; no float arguments
      99	  mov rdi, [8*2+rsp]
     100	  mov rsi, 1
     101	  mov rdx, [rsp]
     102	  call rukaml_applyN
     103	  add rsp, 8 ; deallocate var "__temp14"
     104	  mov [rsp], rax
     105	  sub rsp, 8 ; allocate for var "temp7"
     106	  mov rdi, [8*1+rsp]
     107	  call rukaml_print_int ; short
     108	  mov [rsp], rax
     109	  sub rsp, 8 ; allocate for var "u"
     110	  mov rdx, [rsp+1*8] 
     111	  mov [rsp], rdx ; access a var "temp7"
     112	  mov qword rax,  0
     113	  add rsp, 8 ; deallocate var "u"
     114	  add rsp, 8 ; deallocate var "temp7"
     115	  add rsp, 8 ; deallocate var "temp6"
     116	  add rsp, 8 ; deallocate var "temp5"
     117	  add rsp, 8 ; deallocate var "temp4"
     118	  add rsp, 8 ; deallocate var "temp3"
     119	  pop rbp
     120	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc program.o ../../back_amd64/rukaml_stdlib.o -o program.exe 
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
  $ chmod u+x program.exe && ./program.exe
  48
