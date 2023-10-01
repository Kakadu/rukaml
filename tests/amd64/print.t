  $ cat print.ml | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 --no-start -
  After ANF transformation.
  let prod a b =
    (2 * b)
  let main =
    let temp2 = prod 1  in
      let w = temp2 8  in
        let u = print w  in
          0
  ANF: let prod a b =
         (2 * b)
       let main =
         let temp2 = prod 1  in
           let w = temp2 8  in
             let u = print w  in
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
      17	  mov qword [rsp],  2
      18	  sub rsp, 8 ; allocate for var "__temp4"
      19	  mov rdx, [rsp+5*8] 
      20	  mov [rsp], rdx ; access a var "b"
      21	  mov rax, [8*1+rsp]
      22	  mov rbx, [rsp]
      23	  imul rbx, rax
      24	  mov rax, rbx
      25	  add rsp, 8 ; deallocate var "__temp4"
      26	  add rsp, 8 ; deallocate var "__temp3"
      27	  pop rbp
      28	  ret  ;;;; prod
      29	GLOBAL main
      30	main:
      31	  push rbp
      32	  mov  rbp, rsp
      33	  mov rdi, rsp
      34	  call rukaml_initialize
      35	  sub rsp, 8 ; allocate for var "temp2"
      36		; expected_arity = 2
      37		; formal_arity = 1
      38		; calling "prod"
      39	  sub rsp, 8 ; allocate wrapper for func __temp7
      40	  mov rdi, prod
      41	  mov rsi, 2
      42	  call rukaml_alloc_closure
      43	  mov [rsp], rax
      44	  sub rsp, 8 ; allocate for argument 0 (name = __temp8)
      45	  mov qword [rsp],  1
      46	  mov rdi, [8*1+rsp]
      47	  mov rsi, 1
      48	  mov rdx, [rsp]
      49	  mov al, 0
      50	  call rukaml_applyN
      51	  mov [8*2+rsp], rax
      52	  add rsp, 8 ; deallocate var "__temp8"
      53	  add rsp, 8 ; deallocate var "__temp7"
      54	  sub rsp, 8 ; allocate for var "w"
      55	  sub rsp, 8 ; allocate for var "__temp9"
      56	  mov qword [rsp],  8
      57	  mov rax, 0  ; no float arguments
      58	  mov rdi, [8*2+rsp]
      59	  mov rsi, 1
      60	  mov rdx, [rsp]
      61	  call rukaml_applyN
      62	  add rsp, 8 ; deallocate var "__temp9"
      63	  mov [rsp], rax
      64	  sub rsp, 8 ; allocate for var "u"
      65	  mov rdi, [8*1+rsp]
      66	  mov rax, 0
      67	  call rukaml_print_int ; short
      68	  mov [rsp], rax
      69	  mov qword rax,  0
      70	  add rsp, 8 ; deallocate var "u"
      71	  add rsp, 8 ; deallocate var "w"
      72	  add rsp, 8 ; deallocate var "temp2"
      73	  pop rbp
      74	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc-12 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe 
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
$ ulimit -c 0
  $ chmod u+x program.exe && ./program.exe
  @
  16
  rukaml_print_int
  Segmentation fault (core dumped)
  [139]
