  $ nasm -felf64 helloworld.s -o hello.o
  $ ld -o hello hello.o
  $ chmod u+x hello
  $ ./hello
  hello, world!
  $ echo $?k
  0k
  $ objdump -M intel -D hello.o
  
  hello.o:     file format elf64-x86-64
  
  
  Disassembly of section .data:
  
  0000000000000000 <message>:
     0:	68 65 6c 6c 6f       	push   0x6f6c6c65
     5:	2c 20                	sub    al,0x20
     7:	77 6f                	ja     78 <message+0x78>
     9:	72 6c                	jb     77 <message+0x77>
     b:	64 21 0a             	and    DWORD PTR fs:[rdx],ecx
  
  Disassembly of section .text:
  
  0000000000000000 <_start>:
     0:	b8 01 00 00 00       	mov    eax,0x1
     5:	bf 01 00 00 00       	mov    edi,0x1
     a:	48 be 00 00 00 00 00 	movabs rsi,0x0
    11:	00 00 00 
    14:	ba 0e 00 00 00       	mov    edx,0xe
    19:	0f 05                	syscall
    1b:	b8 3c 00 00 00       	mov    eax,0x3c
    20:	48 31 ff             	xor    rdi,rdi
    23:	0f 05                	syscall
