  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  > let rec fac n =
  >   if n=1 then 1 else n * fac (n-1)
  > let main = fac 5
  > EOF
  After ANF transformation.
  let rec fac n =
    let temp1 = (n = 1) in
      (if temp1
      then 1
      else let temp3 = (n - 1) in
             let temp4 = fac temp3  in
               (n * temp4))
  let main =
    fac 5 
  ANF: let rec fac n =
         let temp1 = (n = 1) in
           (if temp1
           then 1
           else let temp3 = (n - 1) in
                  let temp4 = fac temp3  in
                    (n * temp4))
       let main =
         fac 5 

; generated code for amd64
  $ cat program.asm | nl -ba
       1	section .text
       2	extern rukaml_alloc_closure
       3	extern rukaml_applyN
       4	
       5	_start:
       6	              push    rbp
       7	              mov     rbp, rsp   ; prologue
       8	              call main
       9	              mov rdi, rax    ; rdi stores return code
      10	              mov rax, 60     ; exit syscall
      11	              syscall
      12	
      13		; @[{stack||stack}@]
      14	GLOBAL fac
      15	
      16	fac:
      17	  push rbp
      18	  mov  rbp, rsp
      19	  sub rsp, 8 ; allocate for var "temp1"
      20	  sub rsp, 8 ; allocate for var "__temp3"
      21	  mov rdx, [rsp+4*8] 
      22	  mov [rsp], rdx ; access a var "n"
      23	  sub rsp, 8 ; allocate for var "__temp4"
      24	  mov qword [rsp],  1
      25	  mov rax, [8*1+rsp]
      26	  mov rbx, [rsp]
      27	  cmp rax, rbx
      28	  je lab_7
      29	  mov qword [8*2+rsp], 0
      30	  jmp lab_8
      31	  lab_7:
      32	    mov qword [8*2+rsp], 1
      33	    jmp lab_8
      34	  lab_8:
      35	  add rsp, 8 ; deallocate var "__temp4"
      36	  add rsp, 8 ; deallocate var "__temp3"
      37	  mov rdx, [rsp+0*8] 
      38	  cmp rdx, 0
      39	  je lab_then_9
      40	  mov qword rax,  1
      41	  jmp lab_endif_10
      42	  lab_then_9:
      43	  sub rsp, 8 ; allocate for var "temp3"
      44	  sub rsp, 8 ; allocate for var "__temp5"
      45	  mov rdx, [rsp+5*8] 
      46	  mov [rsp], rdx ; access a var "n"
      47	  mov rax, [rsp]
      48	  dec rax
      49	  mov [8*1+rsp], rax
      50	  add rsp, 8 ; deallocate var "__temp5"
      51	  sub rsp, 8 ; allocate for var "temp4"
      52		; expected_arity = 1
      53		; formal_arity = 1
      54		; calling "fac"
      55	  ; expected_arity = formal_arity = 1
      56	  sub rsp, 8 ; allocate for argument 0 (name = __temp6)
      57	  mov rdx, [rsp+2*8] 
      58	  mov [rsp], rdx ; access a var "temp3"
      59	  call fac
      60	  add rsp, 8 ; deallocate var "__temp6"
      61	  mov [rsp], rax
      62	  sub rsp, 8 ; allocate for var "__temp7"
      63	  mov rdx, [rsp+6*8] 
      64	  mov [rsp], rdx ; access a var "n"
      65	  sub rsp, 8 ; allocate for var "__temp8"
      66	  mov rdx, [rsp+2*8] 
      67	  mov [rsp], rdx ; access a var "temp4"
      68	  mov rax, [8*1+rsp]
      69	  mov rbx, [rsp]
      70	  imul rbx, rax
      71	  mov rax, rbx
      72	  add rsp, 8 ; deallocate var "__temp8"
      73	  add rsp, 8 ; deallocate var "__temp7"
      74	  add rsp, 8 ; deallocate var "temp4"
      75	  add rsp, 8 ; deallocate var "temp3"
      76	  lab_endif_10:
      77	  add rsp, 8 ; deallocate var "temp1"
      78	  pop rbp
      79	  ret  ;;;; fac
      80	
      81		; @[{stack||stack}@]
      82	GLOBAL main
      83	main:
      84	  push rbp
      85	  mov  rbp, rsp
      86		; expected_arity = 1
      87		; formal_arity = 1
      88		; calling "fac"
      89	  ; expected_arity = formal_arity = 1
      90	  sub rsp, 8 ; allocate for argument 0 (name = __temp11)
      91	  mov qword [rsp],  5
      92	  call fac
      93	  add rsp, 8 ; deallocate var "__temp11"
      94	  mov rax, rax
      95	  pop rbp
      96	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  ld: warning: cannot find entry symbol _start; defaulting to 0000000000401000
  [120]
