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
       5	extern rukaml_field
       6	extern rukaml_alloc_pair
       7	extern rukaml_initialize
       8	extern rukaml_gc_compact
       9	extern rukaml_gc_print_stats
      10	
      11	_start:
      12	              push    rbp
      13	              mov     rbp, rsp   ; prologue
      14	              call main
      15	              mov rdi, rax    ; rdi stores return code
      16	              mov rax, 60     ; exit syscall
      17	              syscall
      18	
      19		; @[{stack||stack}@]
      20	GLOBAL fac
      21	
      22	fac:
      23	  push rbp
      24	  mov  rbp, rsp
      25	  sub rsp, 8 ; allocate for var "temp1"
      26	  sub rsp, 8 ; allocate for var "__temp3"
      27	  mov rdx, [rsp+4*8] 
      28	  mov [rsp], rdx ; access a var "n"
      29	  sub rsp, 8 ; allocate for var "__temp4"
      30	  mov qword [rsp],  1
      31	  mov rax, [8*1+rsp]
      32	  mov rbx, [rsp]
      33	  cmp rax, rbx
      34	  je lab_8
      35	  mov qword [8*2+rsp], 0
      36	  jmp lab_9
      37	  lab_8:
      38	    mov qword [8*2+rsp], 1
      39	    jmp lab_9
      40	  lab_9:
      41	  add rsp, 8 ; deallocate var "__temp4"
      42	  add rsp, 8 ; deallocate var "__temp3"
      43	  mov rdx, [rsp+0*8] 
      44	  cmp rdx, 0
      45	  je lab_then_10
      46	  mov qword rax,  1
      47	  jmp lab_endif_11
      48	  lab_then_10:
      49	  sub rsp, 8 ; allocate for var "temp3"
      50	  sub rsp, 8 ; allocate for var "__temp5"
      51	  mov rdx, [rsp+5*8] 
      52	  mov [rsp], rdx ; access a var "n"
      53	  mov rax, [rsp]
      54	  dec rax
      55	  mov [8*1+rsp], rax
      56	  add rsp, 8 ; deallocate var "__temp5"
      57	  sub rsp, 8 ; allocate for var "temp4"
      58		; expected_arity = 1
      59		; formal_arity = 1
      60		; calling "fac"
      61	  ; expected_arity = formal_arity = 1
      62	  sub rsp, 8 ; allocate for argument 0 (name = __temp6)
      63	  mov rdx, [rsp+2*8] 
      64	  mov [rsp], rdx ; access a var "temp3"
      65	  call fac
      66	  add rsp, 8 ; deallocate var "__temp6"
      67	  mov [rsp], rax
      68	  sub rsp, 8 ; allocate for var "__temp7"
      69	  mov rdx, [rsp+6*8] 
      70	  mov [rsp], rdx ; access a var "n"
      71	  sub rsp, 8 ; allocate for var "__temp8"
      72	  mov rdx, [rsp+2*8] 
      73	  mov [rsp], rdx ; access a var "temp4"
      74	  mov rax, [8*1+rsp]
      75	  mov rbx, [rsp]
      76	  imul rbx, rax
      77	  mov rax, rbx
      78	  add rsp, 8 ; deallocate var "__temp8"
      79	  add rsp, 8 ; deallocate var "__temp7"
      80	  add rsp, 8 ; deallocate var "temp4"
      81	  add rsp, 8 ; deallocate var "temp3"
      82	  lab_endif_11:
      83	  add rsp, 8 ; deallocate var "temp1"
      84	  pop rbp
      85	  ret  ;;;; fac
      86	
      87		; @[{stack||stack}@]
      88	GLOBAL main
      89	main:
      90	  push rbp
      91	  mov  rbp, rsp
      92	mov rdi, rsp
      93	call rukaml_initialize
      94	  sub rsp, 8 ; allocate for var "temp6"
      95		; expected_arity = 1
      96		; formal_arity = 1
      97		; calling "fac"
      98	  ; expected_arity = formal_arity = 1
      99	  sub rsp, 8 ; allocate for argument 0 (name = __temp11)
     100	  mov qword [rsp],  5
     101	  call fac
     102	  add rsp, 8 ; deallocate var "__temp11"
     103	  mov [rsp], rax
     104	  sub rsp, 8 ; allocate for var "temp7"
     105	  mov rdi, [8*1+rsp]
     106	  call rukaml_print_int ; short
     107	  mov [rsp], rax
     108	  sub rsp, 8 ; allocate for var "t"
     109	  mov rdx, [rsp+1*8] 
     110	  mov [rsp], rdx ; access a var "temp7"
     111	  mov qword rax,  0
     112	  add rsp, 8 ; deallocate var "t"
     113	  add rsp, 8 ; deallocate var "temp7"
     114	  add rsp, 8 ; deallocate var "temp6"
     115	  pop rbp
     116	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o 
  $ gcc -g -o program.exe ../../back_amd64/rukaml_stdlib.o program.o && ./program.exe
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  120
