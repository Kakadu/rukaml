$ which minimlc
$ ls /home/kakadu/asp/miniml/_build/install/default/bin
$ ls ../../back_amd64/amd64_compiler.exe

# CPS Factorial
$ cat << EOF | ../../back_amd64/amd64_compiler.exe -vamd64 -
> let rec fack n k =
>  if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
> 
> let id2 x = x
> let main = fack 6 id2
> EOF
> let rec fac n =
>   if n=1 then 1 else n * fac (n-1)

  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  > let sq y = y * y
  > let double x = x+x
  > let main = sq 7
  > EOF
  After ANF transformation.
  let sq y =
    (y * y)
  let double x =
    (x + x)
  let main =
    sq 7 
  ANF: let sq y =
         (y * y)
       let double x =
         (x + x)
       let main =
         sq 7 

; generated code for amd64
  $ cat program.asm | nl -ba
       1	section .data
       2	            newline_char: db 10
       3	            codes: db '0123456789abcdef' 
       4	section .text
       5	global _start
       6	print_newline:
       7	          mov rax, 1 ; 'write' syscall identifier
       8	          mov rdi, 1 ; stdout file descriptor
       9	          mov rsi, newline_char ; where do we take data from
      10	          mov rdx, 1 ; the amount of bytes to write
      11	          syscall
      12	          ret 
      13	
      14	print_hex:
      15	  mov rax, rdi
      16	  mov rdi, 1
      17	  mov rdx, 1
      18	  mov rcx, 64 ; how far are we shifting rax?
      19	iterate:
      20	  push rax ; Save the initial rax value
      21	  sub rcx, 4
      22	  sar rax, cl ; shift to 60, 56, 52, ... 4, 0
      23	              ; the cl register is the smallest part of rcx
      24	  and rax, 0xf ; clear all bits but the lowest four
      25	  lea rsi, [codes + rax]; take a hexadecimal digit character code
      26	  mov rax, 1
      27	  push rcx  ; syscall will break rcx
      28	  syscall   ; rax = 1 (31) -- the write identifier,
      29	            ; rdi = 1 for stdout,
      30	            ; rsi = the address of a character, see line 29
      31	  pop rcx
      32	  pop rax          ; ˆ see line 24 ˆ
      33	  test rcx, rcx    ; rcx = 0 when all digits are shown
      34	  jnz iterate
      35	  ret
      36	
      37	_start:
      38	              push    rbp
      39	              mov     rbp, rsp   ; prologue
      40	              call main
      41	              mov rdi, rax    ; rdi stores return code
      42	              mov rax, 60     ; exit syscall
      43	              syscall
      44	
      45	sq:
      46	  push rbp
      47	  mov  rbp, rsp
      48	  sub rsp, 8 ; allocate for var "__temp3"
      49	  mov rdx, [rsp+3*8] 
      50	  mov [0+rsp], rdx ; access a var "y"
      51	  sub rsp, 8 ; allocate for var "__temp4"
      52	  mov rdx, [rsp+4*8] 
      53	  mov [0+rsp], rdx ; access a var "y"
      54	  mov rax, [8+rsp]
      55	  mov rbx, [0+rsp]
      56	  imul rbx, rax
      57	  mov rax, rbx
      58	  add rsp, 8 ; deallocate var "__temp4"
      59	  add rsp, 8 ; deallocate var "__temp3"
      60	  pop rbp
      61	  ret  ;;;; sq
      62	
      63	double:
      64	  push rbp
      65	  mov  rbp, rsp
      66	  sub rsp, 8 ; allocate for var "__temp7"
      67	  mov rdx, [rsp+3*8] 
      68	  mov [0+rsp], rdx ; access a var "x"
      69	  sub rsp, 8 ; allocate for var "__temp8"
      70	  mov rdx, [rsp+4*8] 
      71	  mov [0+rsp], rdx ; access a var "x"
      72	  mov rax, [8+rsp]
      73	  mov rbx, [0+rsp]
      74	  add  rbx, rax
      75	  mov rax, rbx
      76	  add rsp, 8 ; deallocate var "__temp8"
      77	  add rsp, 8 ; deallocate var "__temp7"
      78	  pop rbp
      79	  ret  ;;;; double
      80	
      81	main:
      82	  push rbp
      83	  mov  rbp, rsp
      84	  sub rsp, 8 ; allocate for var "__temp11"
      85	  mov qword [0+rsp],  7
      86	  call sq
      87	  add rsp, 8 ; deallocate var "__temp11"
      88	  pop rbp
      89	  ret  ;;;; main

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
  [49]
