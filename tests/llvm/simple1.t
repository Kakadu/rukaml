  $ cat << EOF | ../../llvm/llvm_compiler.exe -o fack.ll -vllvm
  > let id x = x
  > let main = id 4
  > EOF
  After ANF transformation.
  let id x =
    x
  let main =
    id 4 
  let id x =
    x
  let main =
    id 4 
    formal parameter 0: i64 %0

  $ cat fack.ll | grep 'target datalayout' --invert-match
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "x86_64-pc-linux-gnu"
  
  declare void @myputc(i64)
  
  declare i64 @rukaml_applyN(i64, i64, ...)
  
  declare i64 @rukaml_alloc_closure(i64, i64)
  
  declare i64 @rukaml_alloc_pair(i64, i64)
  
  declare i64 @rukaml_field(i64, i64)
  
  define i64 @id(i64 %0) {
  entry:
    ret i64 %0
  }
  
  define i64 @main() {
  entry:
    %0 = call i64 @id(i64 4)
    ret i64 %0
  }
  $ clang-16 fack.ll ../../compiler/rukaml_stdlib.o -o fack.exe
  $ ./fack.exe
  [4]
