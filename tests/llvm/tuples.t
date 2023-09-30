
  $ cat << EOF | ../../llvm/llvm_compiler.exe -o mydiv.ll #-vllvm
  > let mydiv a b = (a+b, a)
  > let f a b =
  >    let (u,v) = mydiv a b in
  >    u+v
  > let main = f 7 13
  > EOF
  After ANF transformation.
  let mydiv a b =
    let temp1 = (a + b) in
      (temp1, a)
  let f a b =
    let temp3 = mydiv a  in
      let temp5 = temp3 b  in
        let u = field 0 temp5 in
          let v = field 1 temp5 in
            (u + v)
  let main =
    let temp7 = f 7  in
      temp7 13 
  let mydiv a b =
    let temp1 = (a + b) in
      (temp1, a)
  let f a b =
    let temp3 = mydiv a  in
      let temp5 = temp3 b  in
        let u = field 0 temp5 in
          let v = field 1 temp5 in
            (u + v)
  let main =
    let temp7 = f 7  in
      temp7 13 
  $ cat mydiv.ll | grep -E 'target datalayout|ModuleID' --invert-match
  source_filename = "main"
  
  declare void @myputc(i64)
  
  declare i64 @rukaml_applyN(i64, i64, ...)
  
  declare i64 @rukaml_alloc_closure(i64, i64)
  
  declare i64 @rukaml_alloc_pair(i64, i64)
  
  declare i64 @rukaml_field(i64, i64)
  
  define i64 @mydiv(i64 %0, i64 %1) {
  entry:
    %2 = add i64 %0, %1
    %3 = call i64 @rukaml_alloc_pair(i64 %2, i64 %0)
    ret i64 %3
  }
  
  define i64 @f(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rukaml_alloc_closure(i64 ptrtoint (i64 (i64, i64)* @mydiv to i64), i64 2)
    %3 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %2, i64 1, i64 %0)
    %4 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %3, i64 1, i64 %1)
    %5 = call i64 @rukaml_field(i64 0, i64 %4)
    %6 = call i64 @rukaml_field(i64 1, i64 %4)
    %7 = add i64 %5, %6
    ret i64 %7
  }
  
  define i64 @main() {
  entry:
    %0 = call i64 @rukaml_alloc_closure(i64 ptrtoint (i64 (i64, i64)* @f to i64), i64 2)
    %1 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %0, i64 1, i64 7)
    %2 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %1, i64 1, i64 13)
    ret i64 %2
  }
  $ clang-14 mydiv.ll ../../compiler/rukaml_stdlib.o -o mydiv.exe #--verbose
  warning: overriding the module target triple with x86_64-pc-linux-gnu [-Woverride-module]
  1 warning generated.
  $ ./mydiv.exe
  [27]
