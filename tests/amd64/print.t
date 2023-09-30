  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 --no-start -
  > let prod a b = a*b
  > let sum a b = a+b
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
            let u = print temp6  in
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
                 let u = print temp6  in
                   0
 

  $ cat program.asm | grep -v 'section .note.GNU-stack'  | nl -ba
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
      11	GLOBAL prod
      12	
      13	prod:
      14	  push rbp
      15	  mov  rbp, rsp
      16	  sub rsp, 8 ; allocate for var "__temp3"
      17	  mov rdx, [rsp+3*8] 
      18	  mov [rsp], rdx ; access a var "a"
      19	  sub rsp, 8 ; allocate for var "__temp4"
      20	  mov rdx, [rsp+5*8] 
      21	  mov [rsp], rdx ; access a var "b"
      22	  mov rax, [8*1+rsp]
      23	  mov rbx, [rsp]
      24	  imul rbx, rax
      25	  mov rax, rbx
      26	  add rsp, 8 ; deallocate var "__temp4"
      27	  add rsp, 8 ; deallocate var "__temp3"
      28	  pop rbp
      29	  ret  ;;;; prod
      30	GLOBAL sum
      31	sum:
      32	  push rbp
      33	  mov  rbp, rsp
      34	  sub rsp, 8 ; allocate for var "__temp7"
      35	  mov rdx, [rsp+3*8] 
      36	  mov [rsp], rdx ; access a var "a"
      37	  sub rsp, 8 ; allocate for var "__temp8"
      38	  mov rdx, [rsp+5*8] 
      39	  mov [rsp], rdx ; access a var "b"
      40	  mov rax, [8*1+rsp]
      41	  mov rbx, [rsp]
      42	  add  rbx, rax
      43	  mov rax, rbx
      44	  add rsp, 8 ; deallocate var "__temp8"
      45	  add rsp, 8 ; deallocate var "__temp7"
      46	  pop rbp
      47	  ret  ;;;; sum
      48	GLOBAL main
      49	main:
      50	  push rbp
      51	  mov  rbp, rsp
      52	  mov rdi, rsp
      53	  call rukaml_initialize
      54	  sub rsp, 8 ; allocate for var "temp3"
      55	  sub rsp, 8 ; allocate for var "__temp11"
      56	  mov qword [rsp],  0
      57	  sub rsp, 8 ; allocate for var "__temp12"
      58	  mov qword [rsp],  0
      59	  mov rax, [8*1+rsp]
      60	  mov rbx, [rsp]
      61	  cmp rax, rbx
      62	  je lab_8
      63	  mov qword [8*2+rsp], 0
      64	  jmp lab_9
      65	lab_8:
      66	  mov qword [8*2+rsp], 1
      67	  jmp lab_9
      68	lab_9:
      69	  add rsp, 8 ; deallocate var "__temp12"
      70	  add rsp, 8 ; deallocate var "__temp11"
      71	  sub rsp, 8 ; allocate for var "temp4"
      72	  mov rdx, [rsp+1*8] 
      73	  cmp rdx, 0
      74	  je lab_then_10
      75	  mov rdi, prod
      76	  mov rsi, 2
      77	  call rukaml_alloc_closure
      78	  mov [rsp], rax
      79	  jmp lab_endif_11
      80	  lab_then_10:
      81	  mov rdi, sum
      82	  mov rsi, 2
      83	  call rukaml_alloc_closure
      84	  mov [rsp], rax
      85	  lab_endif_11:
      86	  sub rsp, 8 ; allocate for var "temp5"
      87	  sub rsp, 8 ; allocate for var "__temp13"
      88	  mov qword [rsp],  6
      89	  mov rax, 0  ; no float arguments
      90	  mov rdi, [8*2+rsp]
      91	  mov rsi, 1
      92	  mov rdx, [rsp]
      93	  call rukaml_applyN
      94	  add rsp, 8 ; deallocate var "__temp13"
      95	  mov [rsp], rax
      96	  sub rsp, 8 ; allocate for var "temp6"
      97	  sub rsp, 8 ; allocate for var "__temp14"
      98	  mov qword [rsp],  8
      99	  mov rax, 0  ; no float arguments
     100	  mov rdi, [8*2+rsp]
     101	  mov rsi, 1
     102	  mov rdx, [rsp]
     103	  call rukaml_applyN
     104	  add rsp, 8 ; deallocate var "__temp14"
     105	  mov [rsp], rax
     106	  sub rsp, 8 ; allocate for var "u"
     107	  mov rdi, [8*1+rsp]
     108	  call rukaml_print_int ; short
     109	  mov [rsp], rax
     110	  mov qword rax,  0
     111	  add rsp, 8 ; deallocate var "u"
     112	  add rsp, 8 ; deallocate var "temp6"
     113	  add rsp, 8 ; deallocate var "temp5"
     114	  add rsp, 8 ; deallocate var "temp4"
     115	  add rsp, 8 ; deallocate var "temp3"
     116	  pop rbp
     117	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc program.o ../../back_amd64/rukaml_stdlib.o -o program.exe 
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
$ ulimit -c 0
  $ chmod u+x program.exe && ./program.exe
  rukaml_print_int 48
