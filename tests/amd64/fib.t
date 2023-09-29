  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  > let rec fib n =
  >   if n=0 then 0 else if n=1 then 1 else fib (n-2) + fib (n-1)
  > let main = fib 8
  > EOF
  After ANF transformation.
  let rec fib n =
    let temp1 = (n = 0) in
      (if temp1
      then 0
      else let temp3 = (n = 1) in
             (if temp3
             then 1
             else let temp5 = (n - 2) in
                    let temp6 = fib temp5  in
                      let temp7 = (n - 1) in
                        let temp8 = fib temp7  in
                          (temp6 + temp8)))
  let main =
    fib 8 
  ANF: let rec fib n =
         let temp1 = (n = 0) in
           (if temp1
           then 0
           else let temp3 = (n = 1) in
                  (if temp3
                  then 1
                  else let temp5 = (n - 2) in
                         let temp6 = fib temp5  in
                           let temp7 = (n - 1) in
                             let temp8 = fib temp7  in
                               (temp6 + temp8)))
       let main =
         fib 8 

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
      18	GLOBAL fib
      19	
      20	fib:
      21	  push rbp
      22	  mov  rbp, rsp
      23	  sub rsp, 8 ; allocate for var "temp1"
      24	  sub rsp, 8 ; allocate for var "__temp3"
      25	  mov rdx, [rsp+4*8] 
      26	  mov [rsp], rdx ; access a var "n"
      27	  sub rsp, 8 ; allocate for var "__temp4"
      28	  mov qword [rsp],  0
      29	  mov rax, [8*1+rsp]
      30	  mov rbx, [rsp]
      31	  cmp rax, rbx
      32	  je lab_11
      33	  mov qword [8*2+rsp], 0
      34	  jmp lab_12
      35	lab_11:
      36	  mov qword [8*2+rsp], 1
      37	  jmp lab_12
      38	lab_12:
      39	  add rsp, 8 ; deallocate var "__temp4"
      40	  add rsp, 8 ; deallocate var "__temp3"
      41	  mov rdx, [rsp+0*8] 
      42	  cmp rdx, 0
      43	  je lab_then_13
      44	  mov qword rax,  0
      45	  jmp lab_endif_14
      46	  lab_then_13:
      47	  sub rsp, 8 ; allocate for var "temp3"
      48	  sub rsp, 8 ; allocate for var "__temp5"
      49	  mov rdx, [rsp+5*8] 
      50	  mov [rsp], rdx ; access a var "n"
      51	  sub rsp, 8 ; allocate for var "__temp6"
      52	  mov qword [rsp],  1
      53	  mov rax, [8*1+rsp]
      54	  mov rbx, [rsp]
      55	  cmp rax, rbx
      56	  je lab_15
      57	  mov qword [8*2+rsp], 0
      58	  jmp lab_16
      59	lab_15:
      60	  mov qword [8*2+rsp], 1
      61	  jmp lab_16
      62	lab_16:
      63	  add rsp, 8 ; deallocate var "__temp6"
      64	  add rsp, 8 ; deallocate var "__temp5"
      65	  mov rdx, [rsp+0*8] 
      66	  cmp rdx, 0
      67	  je lab_then_17
      68	  mov qword rax,  1
      69	  jmp lab_endif_18
      70	  lab_then_17:
      71	  sub rsp, 8 ; allocate for var "temp5"
      72	  sub rsp, 8 ; allocate for var "__temp7"
      73	  mov rdx, [rsp+6*8] 
      74	  mov [rsp], rdx ; access a var "n"
      75	  mov rax, [rsp]
      76	  mov qword rbx, 2
      77	  sub rax, rbx
      78	  mov [8*1+rsp], rax
      79	  add rsp, 8 ; deallocate var "__temp7"
      80	  sub rsp, 8 ; allocate for var "temp6"
      81		; expected_arity = 1
      82		; formal_arity = 1
      83		; calling "fib"
      84	  ; expected_arity = formal_arity = 1
      85	  sub rsp, 8 ; allocate for argument 0 (name = __temp8)
      86	  mov rdx, [rsp+2*8] 
      87	  mov [rsp], rdx ; access a var "temp5"
      88	  call fib
      89	  add rsp, 8 ; deallocate var "__temp8"
      90	  mov [rsp], rax
      91	  sub rsp, 8 ; allocate for var "temp7"
      92	  sub rsp, 8 ; allocate for var "__temp9"
      93	  mov rdx, [rsp+8*8] 
      94	  mov [rsp], rdx ; access a var "n"
      95	  mov rax, [rsp]
      96	  dec rax
      97	  mov [8*1+rsp], rax
      98	  add rsp, 8 ; deallocate var "__temp9"
      99	  sub rsp, 8 ; allocate for var "temp8"
     100		; expected_arity = 1
     101		; formal_arity = 1
     102		; calling "fib"
     103	  ; expected_arity = formal_arity = 1
     104	  sub rsp, 8 ; allocate for argument 0 (name = __temp10)
     105	  mov rdx, [rsp+2*8] 
     106	  mov [rsp], rdx ; access a var "temp7"
     107	  call fib
     108	  add rsp, 8 ; deallocate var "__temp10"
     109	  mov [rsp], rax
     110	  sub rsp, 8 ; allocate for var "__temp11"
     111	  mov rdx, [rsp+3*8] 
     112	  mov [rsp], rdx ; access a var "temp6"
     113	  sub rsp, 8 ; allocate for var "__temp12"
     114	  mov rdx, [rsp+2*8] 
     115	  mov [rsp], rdx ; access a var "temp8"
     116	  mov rax, [8*1+rsp]
     117	  mov rbx, [rsp]
     118	  add  rbx, rax
     119	  mov rax, rbx
     120	  add rsp, 8 ; deallocate var "__temp12"
     121	  add rsp, 8 ; deallocate var "__temp11"
     122	  add rsp, 8 ; deallocate var "temp8"
     123	  add rsp, 8 ; deallocate var "temp7"
     124	  add rsp, 8 ; deallocate var "temp6"
     125	  add rsp, 8 ; deallocate var "temp5"
     126	  lab_endif_18:
     127	  add rsp, 8 ; deallocate var "temp3"
     128	  lab_endif_14:
     129	  add rsp, 8 ; deallocate var "temp1"
     130	  pop rbp
     131	  ret  ;;;; fib
     132	GLOBAL main
     133	main:
     134	  push rbp
     135	  mov  rbp, rsp
     136	  mov rdi, rsp
     137	  call rukaml_initialize
     138		; expected_arity = 1
     139		; formal_arity = 1
     140		; calling "fib"
     141	  ; expected_arity = formal_arity = 1
     142	  sub rsp, 8 ; allocate for argument 0 (name = __temp15)
     143	  mov qword [rsp],  8
     144	  call fib
     145	  add rsp, 8 ; deallocate var "__temp15"
     146	  mov rax, rax
     147	  pop rbp
     148	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  ld: program.o: in function `main':
  program.asm:(.text+0x1a7): undefined reference to `rukaml_initialize'
  [1]
