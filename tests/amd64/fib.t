  $ cat << EOF | ../../back/amd64/amd64_compiler.exe -o program.asm - -vamd64
  > let rec fib n =
  >   if n=0 then 0 else if n=1 then 1 else fib (n-2) + fib (n-1)
  > let main = let u = print (fib 8) in 0
  > EOF
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
         let temp10 = fib 8  in
           let u = print temp10  in
             0
  Location argument "n" in [rbp+2]
  Removing info about args [ n ]
  Removing info about args [  ]

; generated code for amd64
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
      10	extern rukaml_print_alloc_closure_count
      11	
      12	_start:
      13	              push    rbp
      14	              mov     rbp, rsp   ; prologue
      15	              call main
      16	              mov rdi, rax    ; rdi stores return code
      17	              mov rax, 60     ; exit syscall
      18	              syscall
      19	GLOBAL fib
      20	
      21	fib:
      22	  push rbp
      23	  mov  rbp, rsp
      24	  sub rsp, 8*6 ; allocate for local variables temp8, temp7, temp6, temp5, temp3, temp1
      25	  mov qword r11, [rbp+2*8]
      26	  mov qword r12, 0
      27	  cmp r11, r12
      28	  je lab_12
      29	  mov qword [rbp-1*8], 0
      30	  jmp lab_13
      31	lab_12:
      32	  mov qword [rbp-1*8], 1
      33	  jmp lab_13
      34	lab_13:
      35	  mov qword rdx, [rbp-1*8]
      36	  cmp rdx, 0
      37	  je lab_then_14
      38	  mov qword rax,  0
      39	  jmp lab_endif_15
      40	lab_then_14:
      41	  mov qword r11, [rbp+2*8]
      42	  mov qword r12, 1
      43	  cmp r11, r12
      44	  je lab_16
      45	  mov qword [rbp-2*8], 0
      46	  jmp lab_17
      47	lab_16:
      48	  mov qword [rbp-2*8], 1
      49	  jmp lab_17
      50	lab_17:
      51	  mov qword rdx, [rbp-2*8]
      52	  cmp rdx, 0
      53	  je lab_then_18
      54	  mov qword rax,  1
      55	  jmp lab_endif_19
      56	lab_then_18:
      57	  mov qword r11, [rbp+2*8]
      58	  sub r11, 2
      59	  mov qword [rbp-3*8], r11
      60	  sub rsp, 8 ; trying to save alignment 16 bytes
      61	  sub rsp, 8*1 ; fun arguments
      62	  mov qword r8, [rbp-3*8]  ; arg "temp5"
      63	  mov qword [rsp+0*8], r8
      64	  call fib
      65	  add rsp, 8*2 ; dealloc args
      66	  mov [rbp-4*8], rax
      67	  mov qword r11, [rbp+2*8]
      68	  dec r11
      69	  mov qword [rbp-5*8], r11
      70	  sub rsp, 8 ; trying to save alignment 16 bytes
      71	  sub rsp, 8*1 ; fun arguments
      72	  mov qword r8, [rbp-5*8]  ; arg "temp7"
      73	  mov qword [rsp+0*8], r8
      74	  call fib
      75	  add rsp, 8*2 ; dealloc args
      76	  mov [rbp-6*8], rax
      77	  mov qword r11, [rbp-4*8]
      78	  mov qword r12, [rbp-6*8]
      79	  add  r11, r12
      80	  mov rax, r11
      81	lab_endif_19:
      82	lab_endif_15:
      83	  add rsp, 8*6 ; deallocate local variables temp8, temp7, temp6, temp5, temp3, temp1
      84	  pop rbp
      85	  ret  ;;;; fib
      86	GLOBAL main
      87	main:
      88	  push rbp
      89	  mov  rbp, rsp
      90	  mov rdi, rsp
      91	  call rukaml_initialize
      92	  sub rsp, 8*2 ; allocate for local variables u, temp10
      93	  sub rsp, 8 ; trying to save alignment 16 bytes
      94	  sub rsp, 8*1 ; fun arguments
      95	  mov qword [rsp+0*8], 8 ; constant
      96	  call fib
      97	  add rsp, 8*2 ; dealloc args
      98	  mov [rbp-1*8], rax
      99	  add rsp, -8*2
     100	  mov r11, [rbp-1*8]
     101	  mov qword [rsp], r11
     102	  call rukaml_print_int ; short
     103	  add rsp, 8*2
     104	  mov [rbp-2*8], rax
     105	  mov qword rax,  0
     106	  add rsp, 8*2 ; deallocate local variables u, temp10
     107	  pop rbp
     108	  ret  ;;;; main

$ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 program.o ../../back/amd64/rukaml_stdlib.o -o program.exe
  $ chmod u+x program.exe && ./program.exe
  rukaml_print_int 21
