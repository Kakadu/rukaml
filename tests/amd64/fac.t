  $ cat fac.ml | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  After ANF transformation.
  let fac n =
    (n + 1)
  let main =
    let n = fac 5  in
      let t = print n  in
        0
  ANF: let fac n =
         (n + 1)
       let main =
         let n = fac 5  in
           let t = print n  in
             0

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
      18	GLOBAL fac
      19	
      20	fac:
      21	  push rbp
      22	  mov  rbp, rsp
      23	  sub rsp, 8 ; allocate for var "__temp3"
      24	  mov rdx, [rsp+3*8] 
      25	  mov [rsp], rdx ; access a var "n"
      26	  sub rsp, 8 ; allocate for var "__temp4"
      27	  mov qword [rsp],  1
      28	  mov rax, [8*1+rsp]
      29	  mov rbx, [rsp]
      30	  add  rbx, rax
      31	  mov rax, rbx
      32	  add rsp, 8 ; deallocate var "__temp4"
      33	  add rsp, 8 ; deallocate var "__temp3"
      34	  pop rbp
      35	  ret  ;;;; fac
      36	GLOBAL main
      37	main:
      38	  push rbp
      39	  mov  rbp, rsp
      40	  mov rdi, rsp
      41	  call rukaml_initialize
      42	  sub rsp, 8 ; allocate for var "n"
      43		; expected_arity = 1
      44		; formal_arity = 1
      45		; calling "fac"
      46	  ; expected_arity = formal_arity = 1
      47	  sub rsp, 8 ; allocate for argument 0 (name = __temp7)
      48	  mov qword [rsp],  5
      49	  call fac
      50	  add rsp, 8 ; deallocate var "__temp7"
      51	  mov [rsp], rax
      52	  sub rsp, 8 ; allocate for var "t"
      53	  mov rdi, [8*1+rsp]
      54	  call rukaml_print_int ; short
      55	  mov [rsp], rax
      56	  mov qword rax,  0
      57	  add rsp, 8 ; deallocate var "t"
      58	  add rsp, 8 ; deallocate var "n"
      59	  pop rbp
      60	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o 
  $ gcc -g -o program.exe ../../back_amd64/rukaml_stdlib.o program.o && ./program.exe
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  rukaml_print_int 6
