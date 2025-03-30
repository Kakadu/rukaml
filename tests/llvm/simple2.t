  $ cat << EOF | ./compiler.exe -o fack.ll #-vllvm
  > let id x = x
  > let foo f x = x
  > let main = foo id 5
  > EOF
  After ANF transformation.
  let id x =
    x
  let foo f x =
    x
  let main =
    let temp1 = foo id  in
      temp1 5 
  let id x =
    x
  let foo f x =
    x
  let main =
    let temp1 = foo id  in
      temp1 5 
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
  
  define i64 @foo(i64 %0, i64 %1) {
  entry:
    ret i64 %1
  }
  
  define i64 @main() {
  entry:
    %0 = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @id to i64), i64 1)
    %1 = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @foo to i64), i64 2)
    %2 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %1, i64 1, i64 %0)
    %3 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %2, i64 1, i64 5)
    ret i64 %3
  }
  $ clang-16 fack.ll ../../compiler/rukaml_stdlib.o -o fack.exe
  $ ./fack.exe
  [5]
