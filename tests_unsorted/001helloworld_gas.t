  $ as  helloworld_gas.s -o hello1.o
  $ ld -o hello hello1.o
There we redirect shell's output to separate file, to be able to sed the output
  $ (sh -c ./hello) 2> err.log
  hello, world!
  [139]
  $ cat err.log | sed 's/ (core dumped)//g'
  Segmentation fault
  $ objdump --dwarf=decodedline  -M intel -D hello1.o | sed 's/[ \t]*$//'
  
  hello1.o:     file format elf64-x86-64
  
  
  Disassembly of section .text:
  
  0000000000000000 <_start>:
     0:	48 c7 c0 01 00 00 00 	mov    rax,0x1
     7:	48 c7 c7 01 00 00 00 	mov    rdi,0x1
     e:	48 c7 c6 00 00 00 00 	mov    rsi,0x0
    15:	48 c7 c2 0e 00 00 00 	mov    rdx,0xe
    1c:	0f 05                	syscall
  
  Disassembly of section .data:
  
  0000000000000000 <message>:
     0:	68 65 6c 6c 6f       	push   0x6f6c6c65
     5:	2c 20                	sub    al,0x20
     7:	77 6f                	ja     78 <message+0x78>
     9:	72 6c                	jb     77 <message+0x77>
     b:	64 21 0a             	and    DWORD PTR fs:[rdx],ecx
