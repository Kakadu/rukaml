  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  > let rec fac n =
  >   if n=1 then 1 else n * fac (n-1)
  > let main =
  >   let t = print (fac 6) in
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
      18	GLOBAL fac
      19	
      20	fac:
      21	  push rbp
      22	  mov  rbp, rsp
      23	  sub rsp, 8 ; allocate for var "temp1"
      24	  sub rsp, 8 ; allocate for var "__temp3"
      25	  mov rdx, [rsp+4*8] 
      26	  mov [rsp], rdx ; access a var "n"
      27	  sub rsp, 8 ; allocate for var "__temp4"
      28	  mov qword [rsp],  1
      29	  mov rax, [8*1+rsp]
      30	  mov rbx, [rsp]
      31	  cmp rax, rbx
      32	  je lab_8
      33	  mov qword [8*2+rsp], 0
      34	  jmp lab_9
      35	lab_8:
      36	  mov qword [8*2+rsp], 1
      37	  jmp lab_9
      38	lab_9:
      39	  add rsp, 8 ; deallocate var "__temp4"
      40	  add rsp, 8 ; deallocate var "__temp3"
      41	  mov rdx, [rsp+0*8] 
      42	  cmp rdx, 0
      43	  je lab_then_10
      44	  mov qword rax,  1
      45	  jmp lab_endif_11
      46	  lab_then_10:
      47	  sub rsp, 8 ; allocate for var "temp3"
      48	  sub rsp, 8 ; allocate for var "__temp5"
      49	  mov rdx, [rsp+5*8] 
      50	  mov [rsp], rdx ; access a var "n"
      51	  mov rax, [rsp]
      52	  dec rax
      53	  mov [8*1+rsp], rax
      54	  add rsp, 8 ; deallocate var "__temp5"
      55	  sub rsp, 8 ; allocate for var "temp4"
      56		; expected_arity = 1
      57		; formal_arity = 1
      58		; calling "fac"
      59	  ; expected_arity = formal_arity = 1
      60	  sub rsp, 8 ; allocate for argument 0 (name = __temp6)
      61	  mov rdx, [rsp+2*8] 
      62	  mov [rsp], rdx ; access a var "temp3"
      63	  call fac
      64	  add rsp, 8 ; deallocate var "__temp6"
      65	  mov [rsp], rax
      66	  sub rsp, 8 ; allocate for var "__temp7"
      67	  mov rdx, [rsp+6*8] 
      68	  mov [rsp], rdx ; access a var "n"
      69	  sub rsp, 8 ; allocate for var "__temp8"
      70	  mov rdx, [rsp+2*8] 
      71	  mov [rsp], rdx ; access a var "temp4"
      72	  mov rax, [8*1+rsp]
      73	  mov rbx, [rsp]
      74	  imul rbx, rax
      75	  mov rax, rbx
      76	  add rsp, 8 ; deallocate var "__temp8"
      77	  add rsp, 8 ; deallocate var "__temp7"
      78	  add rsp, 8 ; deallocate var "temp4"
      79	  add rsp, 8 ; deallocate var "temp3"
      80	  lab_endif_11:
      81	  add rsp, 8 ; deallocate var "temp1"
      82	  pop rbp
      83	  ret  ;;;; fac
      84	GLOBAL main
      85	main:
      86	  push rbp
      87	  mov  rbp, rsp
      88	  mov rdi, rsp
      89	  call rukaml_initialize
      90	  sub rsp, 8 ; allocate for var "temp6"
      91		; expected_arity = 1
      92		; formal_arity = 1
      93		; calling "fac"
      94	  ; expected_arity = formal_arity = 1
      95	  sub rsp, 8 ; allocate for argument 0 (name = __temp11)
      96	  mov qword [rsp],  5
      97	  call fac
      98	  add rsp, 8 ; deallocate var "__temp11"
      99	  mov [rsp], rax
     100	  sub rsp, 8 ; allocate for var "temp7"
     101	  mov rdi, [8*1+rsp]
     102	  call rukaml_print_int ; short
     103	  mov [rsp], rax
     104	  sub rsp, 8 ; allocate for var "t"
     105	  mov rdx, [rsp+1*8] 
     106	  mov [rsp], rdx ; access a var "temp7"
     107	  mov qword rax,  0
     108	  add rsp, 8 ; deallocate var "t"
     109	  add rsp, 8 ; deallocate var "temp7"
     110	  add rsp, 8 ; deallocate var "temp6"
     111	  pop rbp
     112	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o 
  $ gcc -g -o program.exe ../../back_amd64/rukaml_stdlib.o program.o && ./program.exe
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  120
