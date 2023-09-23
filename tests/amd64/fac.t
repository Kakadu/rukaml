  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  > let rec fac n =
  >   if n=1 then 1 else n * fac (n-1)
  > let main =
  >   let t = print (fac 5) in
  >   0
  > EOF
  After ANF transformation.
  let rec fac n =
    let temp1 = (n = 1) in
      (if temp1
      then 1
      else let temp3 = (n - 1) in
             let temp4 = fac temp3  in
               (n * temp4))
  let main =
    let temp6 = fac 5  in
      let temp7 = print temp6  in
        let t = temp7 in
          0
  ANF: let rec fac n =
         let temp1 = (n = 1) in
           (if temp1
           then 1
           else let temp3 = (n - 1) in
                  let temp4 = fac temp3  in
                    (n * temp4))
       let main =
         let temp6 = fac 5  in
           let temp7 = print temp6  in
             let t = temp7 in
               0

; generated code for amd64
  $ cat program.asm | nl -ba
       1	section .text
       2	extern rukaml_alloc_closure
       3	extern rukaml_print_int
       4	extern rukaml_applyN
       5	
       6	_start:
       7	              push    rbp
       8	              mov     rbp, rsp   ; prologue
       9	              call main
      10	              mov rdi, rax    ; rdi stores return code
      11	              mov rax, 60     ; exit syscall
      12	              syscall
      13	
      14		; @[{stack||stack}@]
      15	GLOBAL fac
      16	
      17	fac:
      18	  push rbp
      19	  mov  rbp, rsp
      20	  sub rsp, 8 ; allocate for var "temp1"
      21	  sub rsp, 8 ; allocate for var "__temp3"
      22	  mov rdx, [rsp+4*8] 
      23	  mov [rsp], rdx ; access a var "n"
      24	  sub rsp, 8 ; allocate for var "__temp4"
      25	  mov qword [rsp],  1
      26	  mov rax, [8*1+rsp]
      27	  mov rbx, [rsp]
      28	  cmp rax, rbx
      29	  je lab_8
      30	  mov qword [8*2+rsp], 0
      31	  jmp lab_9
      32	  lab_8:
      33	    mov qword [8*2+rsp], 1
      34	    jmp lab_9
      35	  lab_9:
      36	  add rsp, 8 ; deallocate var "__temp4"
      37	  add rsp, 8 ; deallocate var "__temp3"
      38	  mov rdx, [rsp+0*8] 
      39	  cmp rdx, 0
      40	  je lab_then_10
      41	  mov qword rax,  1
      42	  jmp lab_endif_11
      43	  lab_then_10:
      44	  sub rsp, 8 ; allocate for var "temp3"
      45	  sub rsp, 8 ; allocate for var "__temp5"
      46	  mov rdx, [rsp+5*8] 
      47	  mov [rsp], rdx ; access a var "n"
      48	  mov rax, [rsp]
      49	  dec rax
      50	  mov [8*1+rsp], rax
      51	  add rsp, 8 ; deallocate var "__temp5"
      52	  sub rsp, 8 ; allocate for var "temp4"
      53		; expected_arity = 1
      54		; formal_arity = 1
      55		; calling "fac"
      56	  ; expected_arity = formal_arity = 1
      57	  sub rsp, 8 ; allocate for argument 0 (name = __temp6)
      58	  mov rdx, [rsp+2*8] 
      59	  mov [rsp], rdx ; access a var "temp3"
      60	  call fac
      61	  add rsp, 8 ; deallocate var "__temp6"
      62	  mov [rsp], rax
      63	  sub rsp, 8 ; allocate for var "__temp7"
      64	  mov rdx, [rsp+6*8] 
      65	  mov [rsp], rdx ; access a var "n"
      66	  sub rsp, 8 ; allocate for var "__temp8"
      67	  mov rdx, [rsp+2*8] 
      68	  mov [rsp], rdx ; access a var "temp4"
      69	  mov rax, [8*1+rsp]
      70	  mov rbx, [rsp]
      71	  imul rbx, rax
      72	  mov rax, rbx
      73	  add rsp, 8 ; deallocate var "__temp8"
      74	  add rsp, 8 ; deallocate var "__temp7"
      75	  add rsp, 8 ; deallocate var "temp4"
      76	  add rsp, 8 ; deallocate var "temp3"
      77	  lab_endif_11:
      78	  add rsp, 8 ; deallocate var "temp1"
      79	  pop rbp
      80	  ret  ;;;; fac
      81	
      82		; @[{stack||stack}@]
      83	GLOBAL main
      84	main:
      85	  push rbp
      86	  mov  rbp, rsp
      87	  sub rsp, 8 ; allocate for var "temp6"
      88		; expected_arity = 1
      89		; formal_arity = 1
      90		; calling "fac"
      91	  ; expected_arity = formal_arity = 1
      92	  sub rsp, 8 ; allocate for argument 0 (name = __temp11)
      93	  mov qword [rsp],  5
      94	  call fac
      95	  add rsp, 8 ; deallocate var "__temp11"
      96	  mov [rsp], rax
      97	  sub rsp, 8 ; allocate for var "temp7"
      98	  mov rdi, [8*1+rsp]
      99	  call rukaml_print_int ; short
     100	  mov [rsp], rax
     101	  sub rsp, 8 ; allocate for var "t"
     102	  mov rdx, [rsp+1*8] 
     103	  mov [rsp], rdx ; access a var "temp7"
     104	  mov qword rax,  0
     105	  add rsp, 8 ; deallocate var "t"
     106	  add rsp, 8 ; deallocate var "temp7"
     107	  add rsp, 8 ; deallocate var "temp6"
     108	  pop rbp
     109	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o 
  $ gcc -g -o program.exe ../../back_amd64/rukaml_stdlib.o program.o && ./program.exe
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  120
