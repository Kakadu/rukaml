  $ cat fac.ml | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  After ANF transformation.
  let fac n =
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
      10	
      11	_start:
      12	              push    rbp
      13	              mov     rbp, rsp   ; prologue
      14	              call main
      15	              mov rdi, rax    ; rdi stores return code
      16	              mov rax, 60     ; exit syscall
      17	              syscall
      18	
      19	GLOBAL fac
      20	fac:
      21	  push rbp
      22	  mov  rbp, rsp
      23	  sub rsp, 8*3 ; allocate for local variables p2, p, temp1
      24	  sub rsp, 8 ; allocate padding for locals
      25	  mov qword r11, [rbp+2*8]
      26	  mov qword r12, 1
      27	  cmp r11, r12
      28	  je lab_9
      29	  mov qword [rbp-1*8], 0
      30	  jmp lab_10
      31	lab_9:
      32	  mov qword [rbp-1*8], 1
      33	  jmp lab_10
      34	lab_10:
      35	  mov qword rdx, [rbp-1*8]
      36	  cmp rdx, 0
      37	  je lab_then_11
      38	  mov qword rax,  1
      39	  jmp lab_endif_12
      40	lab_then_11:
      41	  mov qword r11, [rbp+2*8]
      42	  dec r11
      43	  mov qword [rbp-2*8], r11
      44	  sub rsp, 8 ; trying to save alignment 16 bytes
      45	  sub rsp, 8*1 ; fun arguments
      46	  mov qword r8, [rbp-2*8]  ; arg "p"
      47	  mov qword [rsp+0*8], r8
      48	  call fac
      49	  add rsp, 8*2 ; dealloc args
      50	  mov [rbp-3*8], rax
      51	  mov qword r11, [rbp+2*8]
      52	  mov qword r12, [rbp-3*8]
      53	  imul r11, r12
      54	  mov rax, r11
      55	lab_endif_12:
      56	  add rsp, 8 ; deallocate padding for locals
      57	  add rsp, 8*3 ; deallocate local variables p2, p, temp1
      58	  pop rbp
      59	  ret  ;;;; fac
      60	
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
      74	  mov rdi, [rbp-1*8]
      75	  call rukaml_print_int ; short
      76	  mov [rbp-2*8], rax
      77	  mov qword rax,  0
      78	  add rsp, 8*2 ; deallocate local variables t, n
      79	  pop rbp
      80	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 -g -o program.exe ../../back_amd64/rukaml_stdlib.o program.o && ./program.exe
  rukaml_print_int 24
