  $ gcc 005print_int.c -c -o print_int.o
  $ nasm -felf64 005fac.s -o fac.o
  005fac.s:11: warning: label alone on a line without a colon might be in error [-w+label-orphan]
  $ gcc fac.o print_int.o -o 005.exe
  /usr/bin/ld: warning: fac.o: missing .note.GNU-stack section implies executable stack
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  $ ./005.exe
  print_int 5
  print_int 720
  $ echo $?
  0
  $ objdump -M intel -D fac.o
  
  fac.o:     file format elf64-x86-64
  
  
  Disassembly of section .text:
  
  0000000000000000 <main>:
     0:	bf 05 00 00 00       	mov    edi,0x5
     5:	e8 00 00 00 00       	call   a <main+0xa>
     a:	6a 06                	push   0x6
     c:	e8 14 00 00 00       	call   25 <fac>
  
  0000000000000011 <popq>:
    11:	48 89 c7             	mov    rdi,rax
    14:	e8 00 00 00 00       	call   19 <popq+0x8>
    19:	bf 00 00 00 00       	mov    edi,0x0
    1e:	b8 3c 00 00 00       	mov    eax,0x3c
    23:	0f 05                	syscall
  
  0000000000000025 <fac>:
    25:	b9 01 00 00 00       	mov    ecx,0x1
    2a:	48 8b 5c 24 08       	mov    rbx,QWORD PTR [rsp+0x8]
  
  000000000000002f <loop>:
    2f:	48 83 fb 01          	cmp    rbx,0x1
    33:	74 09                	je     3e <endloop>
    35:	48 0f af cb          	imul   rcx,rbx
    39:	48 ff cb             	dec    rbx
    3c:	eb f1                	jmp    2f <loop>
  
  000000000000003e <endloop>:
    3e:	48 89 c8             	mov    rax,rcx
    41:	c3                   	ret
