$ ls  ../..
$ ls  ../../llvm
$ ../../compiler.exe -help

# CPS Factorial
$ cat << EOF | ../../llvm/llvm_compiler.exe -o fac.ll
> let rec fack n k =
>   if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
> EOF
  
$ ../../llvm/llvm_compiler.exe -o fac.ll fack.ml
  $ cat << EOF | ../../llvm/llvm_compiler.exe -o fac.ll
  > let fresh_1 m k n = k (n * m)
  > let rec fack k n =
  >   let temp = (n=1) in
  >   if temp
  >     then k 1
  >     else fack ( fresh_1 n k) (n-1)
  > EOF
  $ cat fac.ll
  ; ModuleID = 'main'
  source_filename = "main"
  target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
  
  declare void @myputc(i32)
  
  define i32 @main() {
  entry:
    call void @myputc(i32 48)
    ret i32 0
  }
