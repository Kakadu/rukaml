  $ cat print.ml | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 --no-start -
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
      11	GLOBAL prod
      12
      13	prod:
      14	  push rbp
      15	  mov  rbp, rsp
      16	  mov qword r11, [rbp+4*8]
      17	  imul r11, 2
      18	  mov qword rax, r11
      19	  pop rbp
      20	  ret  ;;;; prod
      21	GLOBAL main
      22	main:
      23	  push rbp
      24	  mov  rbp, rsp
      25	  mov rdi, rsp
      26	  call rukaml_initialize
      27	  sub rsp, 8*4 ; allocate for local variables u, w, temp3, temp2
      28	  mov rdi, prod
      29	  mov rsi, 3
      30	  call rukaml_alloc_closure
      31	  sub rsp, 8 ; trying to save alignment 16 bytes
      32	  sub rsp, 8*1 ; fun arguments
      33	  mov qword [rsp+0*8], 1 ; constant
      34	  mov rdi, rax
      35	  mov rsi, 1
      36	  mov rdx, [rsp+8*0]
      37	  mov al, 0
      38	  call rukaml_applyN
      39	  add rsp, 8*2 ; deallocate args of rukaml_applyN
      40	  mov [rbp-1*8], rax
      41	  sub rsp, 8 ; padding
      42	  sub rsp, 8 ; first arg of a function temp2
      43	  mov qword [rbp-6*8],  8
      44	  mov rax, 0  ; no float arguments
      45	  mov rdi, [rbp-1*8]
      46	  mov rsi, 1
      47	  mov rdx, [rbp-6*8]
      48	  call rukaml_applyN
      49	  add rsp, 8*2 ; free space for args of function "temp2"
      50	  mov [rbp-2*8], rax
      51	  sub rsp, 8 ; padding
      52	  sub rsp, 8 ; first arg of a function temp3
      53	  mov qword [rbp-6*8],  10
      54	  mov rax, 0  ; no float arguments
      55	  mov rdi, [rbp-2*8]
      56	  mov rsi, 1
      57	  mov rdx, [rbp-6*8]
      58	  call rukaml_applyN
      59	  add rsp, 8*2 ; free space for args of function "temp3"
      60	  mov [rbp-3*8], rax
      61	  mov rdi, [rbp-3*8]
      62	  call rukaml_print_int ; short
      63	  mov [rbp-4*8], rax
      64	  mov qword rax,  0
      65	  add rsp, 8*4 ; deallocate local variables u, w, temp3, temp2
      66	  pop rbp
      67	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc-13 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE

$ ulimit -c 0
  $ chmod u+x program.exe && ./program.exe
  rukaml_print_int 20
