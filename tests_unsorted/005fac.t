  $ gcc 005print_int.c -c -o print_int.o
  $ nasm -felf64 005fac.s -o fac.o
  $ gcc fac.o print_int.o -o 005.exe
  /usr/bin/ld: warning: fac.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  $ ./005.exe
  print_int 24
  $ echo $?
  0
  $ objdump -M intel -D fac.o
  
  fac.o:     file format elf64-x86-64
  
  
  Disassembly of section .text:
  
  0000000000000000 <_start>:
     0:	55                   	push   rbp
     1:	48 89 e5             	mov    rbp,rsp
     4:	e8 38 00 00 00       	call   41 <main>
     9:	48 89 c7             	mov    rdi,rax
     c:	b8 3c 00 00 00       	mov    eax,0x3c
    11:	0f 05                	syscall
  
  0000000000000013 <fac>:
    13:	48 83 ec 18          	sub    rsp,0x18
    17:	89 7c 24 0c          	mov    DWORD PTR [rsp+0xc],edi
    1b:	83 7c 24 0c 01       	cmp    DWORD PTR [rsp+0xc],0x1
    20:	7e 15                	jle    37 <fac.L2>
    22:	8b 44 24 0c          	mov    eax,DWORD PTR [rsp+0xc]
    26:	83 e8 01             	sub    eax,0x1
    29:	89 c7                	mov    edi,eax
    2b:	e8 e3 ff ff ff       	call   13 <fac>
    30:	0f af 44 24 0c       	imul   eax,DWORD PTR [rsp+0xc]
    35:	eb 05                	jmp    3c <fac.L4>
  
  0000000000000037 <fac.L2>:
    37:	b8 01 00 00 00       	mov    eax,0x1
  
  000000000000003c <fac.L4>:
    3c:	48 83 c4 18          	add    rsp,0x18
    40:	c3                   	ret
  
  0000000000000041 <main>:
    41:	55                   	push   rbp
    42:	48 89 e5             	mov    rbp,rsp
    45:	bf 04 00 00 00       	mov    edi,0x4
    4a:	e8 c4 ff ff ff       	call   13 <fac>
    4f:	89 c7                	mov    edi,eax
    51:	e8 00 00 00 00       	call   56 <main+0x15>
    56:	5d                   	pop    rbp
    57:	c3                   	ret
