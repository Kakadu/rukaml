$ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
> let kont1 n k a = k (n*a)
> let rec fac n k =
>   if n=1 then k 1 else fac (n-1) (kont1 n k)
> let id y = y
> let main = fac 5 id
> EOF
$ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
> let sum a b = a+b
> let prod a b = a * b
> let app f x y = f x y
> let foo n  = if n=0 then sum else prod
> let main = app (foo 5) 10 20
> EOF
  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 --no-start -
  > let sum a b = a+b
  > let prod a b = a * b
  > let main = (if 0=1 then sum else prod) 10 204
  > EOF
  After ANF transformation.
  let sum a b =
    (a + b)
  let main =
    let temp2 = sum 10  in
      temp2 204 
  ANF: let sum a b =
         (a + b)
       let main =
         let temp2 = sum 10  in
           temp2 204 

; generated code for amd64
  $ cat program.asm | nl -ba
       1	section .text
       2	extern rukaml_alloc_closure
       3	extern rukaml_applyN
       4	
       5	
       6		; @[{stack||stack}@]
       7	GLOBAL sum
       8	
       9	sum:
      10	  push rbp
      11	  mov  rbp, rsp
      12	  sub rsp, 8 ; allocate for var "__temp3"
      13	  mov rdx, [rsp+4*8] 
      14	  mov [rsp], rdx ; access a var "a"
      15	  sub rsp, 8 ; allocate for var "__temp4"
      16	  mov rdx, [rsp+4*8] 
      17	  mov [rsp], rdx ; access a var "b"
      18	  mov rax, [8*1+rsp]
      19	  mov rbx, [rsp]
      20	  add  rbx, rax
      21	  mov rax, rbx
      22	  add rsp, 8 ; deallocate var "__temp4"
      23	  add rsp, 8 ; deallocate var "__temp3"
      24	  pop rbp
      25	  ret  ;;;; sum
      26	
      27		; @[{stack||stack}@]
      28	GLOBAL main
      29	main:
      30	  push rbp
      31	  mov  rbp, rsp
      32	  sub rsp, 8 ; allocate for var "temp2"
      33		; expected_arity = 2
      34		; formal_arity = 1
      35		; calling "sum"
      36	  sub rsp, 8 ; allocate wrapper for func __temp7
      37	  mov rdi, sum
      38	  mov rsi, 2
      39	  call rukaml_alloc_closure
      40	  mov [rsp], rax
      41	  sub rsp, 8 ; allocate for argument 0 (name = __temp8)
      42	  mov qword [rsp],  10
      43	  mov rdi, [8*1+rsp]
      44	  mov rsi, 1
      45	  mov rdx, [rsp]
      46	  mov al, 0
      47	  call rukaml_applyN
      48	  mov [8*2+rsp], rax
      49	  add rsp, 8 ; deallocate var "__temp8"
      50	  add rsp, 8 ; deallocate var "__temp7"
      51	  sub rsp, 8 ; allocate for var "__temp9"
      52	  mov qword [rsp],  204
      53	  mov rax, 0  ; no float arguments
      54	  mov rdi, [8*1+rsp]
      55	  mov rsi, 1
      56	  mov rdx, [rsp]
      57	  call rukaml_applyN
      58	  add rsp, 8 ; deallocate var "__temp9"
      59	  mov rax, rax
      60	  add rsp, 8 ; deallocate var "temp2"
      61	  pop rbp
      62	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
  $ gcc program.o ../../back_amd64/rukaml_stdlib.o -o program.exe 
  /usr/bin/ld: warning: program.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
  $ chmod u+x program.exe && ./program.exe
  [214]
