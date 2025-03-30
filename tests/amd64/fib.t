  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
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
      23	  sub rsp, 8*6 ; allocate for local variables temp8, temp7, temp6, temp5, temp3, temp1
      24	  mov qword r11, [rbp+2*8]
      25	  mov qword r12, 0
      26	  cmp r11, r12
      27	  je lab_12
      28	  mov qword [rbp-1*8], 0
      29	  jmp lab_13
      30	lab_12:
      31	  mov qword [rbp-1*8], 1
      32	  jmp lab_13
      33	lab_13:
      34	  mov qword rdx, [rbp-1*8]
      35	  cmp rdx, 0
      36	  je lab_then_14
      37	  mov qword rax,  0
      38	  jmp lab_endif_15
      39	lab_then_14:
      40	  mov qword r11, [rbp+2*8]
      41	  mov qword r12, 1
      42	  cmp r11, r12
      43	  je lab_16
      44	  mov qword [rbp-2*8], 0
      45	  jmp lab_17
      46	lab_16:
      47	  mov qword [rbp-2*8], 1
      48	  jmp lab_17
      49	lab_17:
      50	  mov qword rdx, [rbp-2*8]
      51	  cmp rdx, 0
      52	  je lab_then_18
      53	  mov qword rax,  1
      54	  jmp lab_endif_19
      55	lab_then_18:
      56	  mov qword r11, [rbp+2*8]
      57	  sub r11, 2
      58	  mov qword [rbp-3*8], r11
      59	  sub rsp, 8 ; trying to save alignment 16 bytes
      60	  sub rsp, 8*1 ; fun arguments
      61	  mov qword r8, [rbp-3*8]  ; arg "temp5"
      62	  mov qword [rsp+0*8], r8
      63	  call fib
      64	  add rsp, 8*2 ; dealloc args
      65	  mov [rbp-4*8], rax
      66	  mov qword r11, [rbp+2*8]
      67	  dec r11
      68	  mov qword [rbp-5*8], r11
      69	  sub rsp, 8 ; trying to save alignment 16 bytes
      70	  sub rsp, 8*1 ; fun arguments
      71	  mov qword r8, [rbp-5*8]  ; arg "temp7"
      72	  mov qword [rsp+0*8], r8
      73	  call fib
      74	  add rsp, 8*2 ; dealloc args
      75	  mov [rbp-6*8], rax
      76	  mov qword r11, [rbp-4*8]
      77	  mov qword r12, [rbp-6*8]
      78	  add  r11, r12
      79	  mov rax, r11
      80	lab_endif_19:
      81	lab_endif_15:
      82	  add rsp, 8*6 ; deallocate local variables temp8, temp7, temp6, temp5, temp3, temp1
      83	  pop rbp
      84	  ret  ;;;; fib
      85	GLOBAL main
      86	main:
      87	  push rbp
      88	  mov  rbp, rsp
      89	  mov rdi, rsp
      90	  call rukaml_initialize
      91	  sub rsp, 8*2 ; allocate for local variables u, temp10
      92	  sub rsp, 8 ; trying to save alignment 16 bytes
      93	  sub rsp, 8*1 ; fun arguments
      94	  mov qword [rsp+0*8], 8 ; constant
      95	  call fib
      96	  add rsp, 8*2 ; dealloc args
      97	  mov [rbp-1*8], rax
      98	  add rsp, -8*2
      99	  mov r11, [rbp-1*8]
     100	  mov qword [rsp], r11
     101	  call rukaml_print_int ; short
     102	  add rsp, 8*2
     103	  mov [rbp-2*8], rax
     104	  mov qword rax,  0
     105	  add rsp, 8*2 ; deallocate local variables u, temp10
     106	  pop rbp
     107	  ret  ;;;; main

$ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe
  $ chmod u+x program.exe && ./program.exe
  rukaml_print_int 21
