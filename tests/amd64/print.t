  $ cat print.ml | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -c -
  After ANF transformation.
  let prod a b c =
    (2 * c)
  let main =
    let temp2 = prod 1  in
      let temp3 = temp2 8  in
        let w = temp3 10  in
          let u = print w  in
            0
  ANF: let prod a b c =
         (2 * c)
       let main =
         let temp2 = prod 1  in
           let temp3 = temp2 8  in
             let w = temp3 10  in
               let u = print w  in
                 0
  Location argument "c" in [rbp+4]
  Location argument "b" in [rbp+3]
  Location argument "a" in [rbp+2]
  Removing info about args [ a b c ]
  Removing info about args [  ]



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
      11	
      12	GLOBAL prod
      13	prod:
      14	  push rbp
      15	  mov  rbp, rsp
      16	  mov qword r11, [rbp+4*8]
      17	  imul r11, 2
      18	  mov qword rax, r11
      19	  pop rbp
      20	  ret  ;;;; prod
      21	
      22	GLOBAL main
      23	main:
      24	  push rbp
      25	  mov  rbp, rsp
      26	  mov rdi, rsp
      27	  call rukaml_initialize
      28	  sub rsp, 8*4 ; allocate for local variables u, w, temp3, temp2
      29	  mov rdi, prod
      30	  mov rsi, 3
      31	  call rukaml_alloc_closure
      32	  sub rsp, 8 ; trying to save alignment 16 bytes
      33	  sub rsp, 8*1 ; fun arguments
      34	  mov qword [rsp+0*8], 1 ; constant
      35	  mov rdi, rax
      36	  mov rsi, 1
      37	  mov rdx, [rsp+8*0]
      38	  mov al, 0
      39	  call rukaml_applyN
      40	  add rsp, 8*2 ; deallocate args of rukaml_applyN
      41	  mov [rbp-1*8], rax
      42	  sub rsp, 8 ; padding
      43	  sub rsp, 8 ; first arg of a function temp2
      44	  mov qword [rbp-6*8],  8
      45	  mov rax, 0  ; no float arguments
      46	  mov rdi, [rbp-1*8]
      47	  mov rsi, 1
      48	  mov rdx, [rbp-6*8]
      49	  call rukaml_applyN
      50	  add rsp, 8*2 ; free space for args of function "temp2"
      51	  mov [rbp-2*8], rax
      52	  sub rsp, 8 ; padding
      53	  sub rsp, 8 ; first arg of a function temp3
      54	  mov qword [rbp-6*8],  10
      55	  mov rax, 0  ; no float arguments
      56	  mov rdi, [rbp-2*8]
      57	  mov rsi, 1
      58	  mov rdx, [rbp-6*8]
      59	  call rukaml_applyN
      60	  add rsp, 8*2 ; free space for args of function "temp3"
      61	  mov [rbp-3*8], rax
      62	  mov rdi, [rbp-3*8]
      63	  call rukaml_print_int ; short
      64	  mov [rbp-4*8], rax
      65	  mov qword rax,  0
      66	  add rsp, 8*4 ; deallocate local variables u, w, temp3, temp2
      67	  pop rbp
      68	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE

$ ulimit -c 0
  $ chmod u+x program.exe && ./program.exe
  rukaml_print_int 20
