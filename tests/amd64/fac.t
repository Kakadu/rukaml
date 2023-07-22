> let rec fac n =
>   if n=1 then 1 else n * fac (n-1)
> let foo n a b = if n = 0 then a else b 
  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm -vamd64 -
  > let main = if 0=0 then 10 else 20
  > EOF

; generated code for amd64
  $ cat program.asm | nl -ba

  $ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
