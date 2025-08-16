  $ cat fac.ml | ../../back/amd64/amd64_compiler.exe -o program.asm - -vamd64
  ANF: let fac n =
         let temp1 = (n = 1) in
           (if temp1
           then 1
           else let p = (n - 1) in
                  let p2 = fac p  in
                    (n * p2))
       let main =
         let n = fac 4  in
           let t = print n  in
             0
  Location argument "n" in [rbp+2]
  Removing info about args [ n ]
  Removing info about args [  ]

; generated code for amd64
  $ cat program.asm | grep -v 'section .note.GNU-stack' | nl -ba
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
      19	GLOBAL fac
      20	
      21	fac:
      22	  push rbp
      23	  mov  rbp, rsp
      24	  sub rsp, 8*3 ; allocate for local variables p2, p, temp1
      25	  sub rsp, 8 ; allocate padding for locals
      26	  mov qword r11, [rbp+2*8]
      27	  mov qword r12, 1
      28	  cmp r11, r12
      29	  je lab_9
      30	  mov qword [rbp-1*8], 0
      31	  jmp lab_10
      32	lab_9:
      33	  mov qword [rbp-1*8], 1
      34	  jmp lab_10
      35	lab_10:
      36	  mov qword rdx, [rbp-1*8]
      37	  cmp rdx, 0
      38	  je lab_then_11
      39	  mov qword rax,  1
      40	  jmp lab_endif_12
      41	lab_then_11:
      42	  mov qword r11, [rbp+2*8]
      43	  dec r11
      44	  mov qword [rbp-2*8], r11
      45	  sub rsp, 8 ; trying to save alignment 16 bytes
      46	  sub rsp, 8*1 ; fun arguments
      47	  mov qword r8, [rbp-2*8]  ; arg "p"
      48	  mov qword [rsp+0*8], r8
      49	  call fac
      50	  add rsp, 8*2 ; dealloc args
      51	  mov [rbp-3*8], rax
      52	  mov qword r11, [rbp+2*8]
      53	  mov qword r12, [rbp-3*8]
      54	  imul r11, r12
      55	  mov rax, r11
      56	lab_endif_12:
      57	  add rsp, 8 ; deallocate padding for locals
      58	  add rsp, 8*3 ; deallocate local variables p2, p, temp1
      59	  pop rbp
      60	  ret  ;;;; fac
      61	GLOBAL main
      62	main:
      63	  push rbp
      64	  mov  rbp, rsp
      65	  mov rdi, rsp
      66	  call rukaml_initialize
      67	  sub rsp, 8*2 ; allocate for local variables t, n
      68	  sub rsp, 8 ; trying to save alignment 16 bytes
      69	  sub rsp, 8*1 ; fun arguments
      70	  mov qword [rsp+0*8], 4 ; constant
      71	  call fac
      72	  add rsp, 8*2 ; dealloc args
      73	  mov [rbp-1*8], rax
      74	  add rsp, -8*2
      75	  mov r11, [rbp-1*8]
      76	  mov qword [rsp], r11
      77	  call rukaml_print_int ; short
      78	  add rsp, 8*2
      79	  mov [rbp-2*8], rax
      80	  mov qword rax,  0
      81	  add rsp, 8*2 ; deallocate local variables t, n
      82	  pop rbp
      83	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 -g -o program.exe ../../back/amd64/rukaml_stdlib.o program.o && ./program.exe
  rukaml_print_int 24
