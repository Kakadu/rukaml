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
  > let main = sum 10 204
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
       3	extern rukaml_apply1
       4	extern rukaml_apply2
       5	extern rukaml_apply3
       6	
       7	
       8		; @[{stack||stack}@]
       9	
      10	sum:
      11	  push rbp
      12	  mov  rbp, rsp
      13	  sub rsp, 8 ; allocate for var "__temp3"
      14	  mov rdx, [rsp+4*8] 
      15	  mov [rsp], rdx ; access a var "a"
      16	  sub rsp, 8 ; allocate for var "__temp4"
      17	  mov rdx, [rsp+4*8] 
      18	  mov [rsp], rdx ; access a var "b"
      19	  mov rax, [8*1+rsp]
      20	  mov rbx, [rsp]
      21	  add  rbx, rax
      22	  mov rax, rbx
      23	  add rsp, 8 ; deallocate var "__temp4"
      24	  add rsp, 8 ; deallocate var "__temp3"
      25	  pop rbp
      26	  ret  ;;;; sum
      27	
      28		; @[{stack||stack}@]
      29	main:
      30	  push rbp
      31	  mov  rbp, rsp
      32	  sub rsp, 8 ; allocate for var "temp2"
      33		; expected_arity = 2
      34		; formal_arity = 1
      35		; calling "sum"
      36	  sub rsp, 8 ; allocate wrapper for func __temp7
      37	  mov rdi, sum
      38	  mov rsi, 1
      39	  call rukaml_alloc_closure
      40	  mov [rsp], eax
      41	  sub rsp, 8 ; allocate for argument 0 (name = __temp8)
      42	  mov qword [rsp],  10
      43	  mov rdi, [8*1+rsp]
      44	  mov rsi, [rsp]
      45	  call rukaml_apply1
      46	  mov [8*2+rsp], eax
      47	  add rsp, 8 ; deallocate var "__temp8"
      48	  add rsp, 8 ; deallocate var "__temp7"
      49	  sub rsp, 8 ; allocate for var "__temp9"
      50	  mov qword [rsp],  204
      51	  mov rdi, [8*1+rsp]
      52	mov rsi, [rsp]
      53	  call rukaml_apply1
      54	  add rsp, 8 ; deallocate var "__temp9"
      55	  mov rax, rax
      56	  add rsp, 8 ; deallocate var "temp2"
      57	  pop rbp
      58	  ret  ;;;; main
  $ nasm -felf64 program.asm -o program.o
$ ld -o program.exe program.o  ../../compiler/rukaml_stdlib.o  -lc #-dynamic-linker /lib/ld-linux.so.2
$ ld -dynamic-linker /lib/ld-linux.so.2 -o program.exe /lib/x86_64-linux-gnu/crt1.o /lib/x86_64-linux-gnu/crti.o /usr/lib/gcc/x86_64-linux-gnu/12/crtbeginT.o ../../compiler/rukaml_stdlib.o program.o -L/usr/lib/gcc/x86_64-linux-gnu/12/ -lgcc -lgcc_eh /usr/bin/../lib/gcc/x86_64-linux-gnu/12/crtend.o /lib/x86_64-linux-gnu/crtn.o
$ ld -o program.exe /lib/x86_64-linux-gnu/crt1.o /lib/x86_64-linux-gnu/crti.o /usr/lib/gcc/x86_64-linux-gnu/12/crtbeginT.o ../../compiler/rukaml_stdlib.o program.o /usr/bin/../lib/gcc/x86_64-linux-gnu/12/crtend.o
$ /usr/bin/ld -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -static -o program.exe ../../compiler/rukaml_stdlib.o /lib/x86_64-linux-gnu/crt1.o /lib/x86_64-linux-gnu/crti.o /usr/lib/gcc/x86_64-linux-gnu/12/crtbeginT.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/12 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/12/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/lib -L/usr/lib   program.o   --start-group -lgcc -lgcc_eh -lc --end-group /usr/bin/../lib/gcc/x86_64-linux-gnu/12/crtend.o /lib/x86_64-linux-gnu/crtn.o 

$ chmod u+x program.exe && ./program.exe
