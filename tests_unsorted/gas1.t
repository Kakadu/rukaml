  $ as hello1.s -o hello1.o
$ ls
  $ objdump -M intel -D hello1.o
  
  hello1.o:     file format elf64-x86-64
  
  
  Disassembly of section .text:
  
  0000000000000000 <.text>:
     0:	48 c7 c0 01 00 00 00 	mov    rax,0x1
     7:	48 c7 c7 01 00 00 00 	mov    rdi,0x1
     e:	48 8d 35 0a 00 00 00 	lea    rsi,[rip+0xa]        # 0x1f
    15:	48 c7 c2 11 00 00 00 	mov    rdx,0x11
    1c:	0f 05                	syscall
    1e:	c3                   	ret
    1f:	48                   	rex.W
    20:	65 6c                	gs ins BYTE PTR es:[rdi],dx
    22:	6c                   	ins    BYTE PTR es:[rdi],dx
    23:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    24:	2c 20                	sub    al,0x20
    26:	59                   	pop    rcx
    27:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    28:	75 72                	jne    0x9c
    2a:	20 4e 61             	and    BYTE PTR [rsi+0x61],cl
    2d:	6d                   	ins    DWORD PTR es:[rdi],dx
    2e:	65 0a 00             	or     al,BYTE PTR gs:[rax]
