  $ nasm -felf64 helloworld_nasm.s -o hello.o
  $ ld -o hello hello.o
  $ chmod u+x hello
There we redirect shell's output to separate file, to be able to sed the output
  $ (sh -c ./hello) 2> err.log
  hello, world!
  [139]
  $ cat err.log | sed 's/ (core dumped)//g'
  Segmentation fault
  $ objdump -M intel -D hello.o | sed 's/[ \t]*$//'
  
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
