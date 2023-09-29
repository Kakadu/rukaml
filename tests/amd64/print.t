  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 --no-start -
  > let prod a b = 48
  > let main =
  >   let u = print (prod 6 8) in 
  >   0
  > EOF
  After ANF transformation.
  let prod a b =
    48
  let main =
    let temp1 = prod 6  in
      let temp2 = temp1 8  in
        let temp3 = print temp2  in
          let u = temp3 in
            0
  ANF: let prod a b =
         48
       let main =
         let temp1 = prod 6  in
           let temp2 = temp1 8  in
             let temp3 = print temp2  in
               let u = temp3 in
                 0
 

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
      11	
      12		; @[{stack||stack}@]
      13	GLOBAL prod
      14	
      15	prod:
      16	  push rbp
      17	  mov  rbp, rsp
      18	  mov qword rax,  48
      19	  pop rbp
      20	  ret  ;;;; prod
      21	
      22		; @[{stack||stack}@]
      23	GLOBAL main
      24	main:
      25	  push rbp
      26	  mov  rbp, rsp
      27	mov rdi, rsp
      28	call rukaml_initialize
      29	  sub rsp, 8 ; allocate for var "temp1"
      30		; expected_arity = 2
      31		; formal_arity = 1
      32		; calling "prod"
      33	  sub rsp, 8 ; allocate wrapper for func __temp5
      34	  mov rdi, prod
      35	  mov rsi, 2
      36	  call rukaml_alloc_closure
      37	  mov [rsp], rax
      38	  sub rsp, 8 ; allocate for argument 0 (name = __temp6)
      39	  mov qword [rsp],  6
      40	  mov rdi, [8*1+rsp]
      41	  mov rsi, 1
      42	  mov rdx, [rsp]
      43	  mov al, 0
      44	  call rukaml_applyN
      45	  mov [8*2+rsp], rax
      46	  add rsp, 8 ; deallocate var "__temp6"
      47	  add rsp, 8 ; deallocate var "__temp5"
      48	  sub rsp, 8 ; allocate for var "temp2"
      49	  sub rsp, 8 ; allocate for var "__temp7"
      50	  mov qword [rsp],  8
      51	  mov rax, 0  ; no float arguments
      52	  mov rdi, [8*2+rsp]
      53	  mov rsi, 1
      54	  mov rdx, [rsp]
      55	  call rukaml_applyN
      56	  add rsp, 8 ; deallocate var "__temp7"
      57	  mov [rsp], rax
      58	  sub rsp, 8 ; allocate for var "temp3"
      59	  mov rdi, [8*1+rsp]
      60	  call rukaml_print_int ; short
      61	  mov [rsp], rax
      62	  sub rsp, 8 ; allocate for var "u"
      63	  mov rdx, [rsp+1*8] 
      64	  mov [rsp], rdx ; access a var "temp3"
      65	  mov qword rax,  0
      66	  add rsp, 8 ; deallocate var "u"
      67	  add rsp, 8 ; deallocate var "temp3"
      68	  add rsp, 8 ; deallocate var "temp2"
      69	  add rsp, 8 ; deallocate var "temp1"
      70	  pop rbp
      71	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc program.o ../../back_amd64/rukaml_stdlib.o -o program.exe 
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
$ ulimit -c 0
  $ chmod u+x program.exe && ./program.exe
  Segmentation fault (core dumped)
  [139]
