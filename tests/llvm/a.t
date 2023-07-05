

# CPS Factorial
  $ cat << EOF | ../../llvm/llvm_compiler.exe -o fack.ll #-vllvm
  > let fresh_1 m k n = k (n * m)
  > let rec fack k n =
  >   if n=1
  >     then k 1
  >     else let u = n-1 in
  >          let v = fresh_1 n k in
  >          fack v u
  > let id x = x
  > let main = fack id 4
  > EOF
  After ANF transformation.
  let fresh_1 m k n =
    let temp4 = (n * m) in
      k temp4 
  let rec fack k n =
    let temp8 = (n = 1) in
      (if temp8
      then k 1 
      else let temp10 = (n - 1) in
             let u = temp10 in
               let temp11 = fresh_1 n  in
                 let temp12 = temp11 k  in
                   let v = temp12 in
                     let temp13 = fack v  in
                       temp13 u )
  let id x =
    x
  let main =
    let temp17 = fack id  in
      temp17 4 
  let fresh_1 m k n =
    let temp4 = (n * m) in
      k temp4 
  let rec fack k n =
    let temp8 = (n = 1) in
      (if temp8
      then k 1 
      else let temp10 = (n - 1) in
             let u = temp10 in
               let temp11 = fresh_1 n  in
                 let temp12 = temp11 k  in
                   let v = temp12 in
                     let temp13 = fack v  in
                       temp13 u )
  let id x =
    x
  let main =
    let temp17 = fack id  in
      temp17 4 

  $ cat fack.ll | grep 'target datalayout' --invert-match
  ; ModuleID = 'main'
  source_filename = "main"
  
  declare void @myputc(i64)
  
  declare i64 @rukaml_applyN(i64, i64, ...)
  
  declare i64 @rukaml_alloc_closure(i64, i64)
  
  define i64 @fresh_1(i64 %0, i64 %1, i64 %2) {
  entry:
    %3 = mul i64 %2, %0
    %4 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %1, i64 1, i64 %3)
    ret i64 %4
  }
  
  define i64 @fack(i64 %0, i64 %1) {
  entry:
    %2 = icmp eq i64 %1, 1
    %name = zext i1 %2 to i64
    %ifcond = icmp ne i64 %name, 0
    br i1 %ifcond, label %fack_then, label %fack_else
  
  fack_cont:                                        ; preds = %fack_else, %fack_then
    %phi_result = phi i64 [ %10, %fack_then ], [ %9, %fack_else ]
    ret i64 %phi_result
  
  fack_else:                                        ; preds = %entry
    %3 = sub i64 %1, 1
    %4 = call i64 @rukaml_alloc_closure(i64 ptrtoint (i64 (i64, i64, i64)* @fresh_1 to i64), i64 3)
    %5 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %4, i64 1, i64 %1)
    %6 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %5, i64 1, i64 %0)
    %7 = call i64 @rukaml_alloc_closure(i64 ptrtoint (i64 (i64, i64)* @fack to i64), i64 2)
    %8 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %7, i64 1, i64 %6)
    %9 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %8, i64 1, i64 %3)
    br label %fack_cont
  
  fack_then:                                        ; preds = %entry
    %10 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %0, i64 1, i64 1)
    br label %fack_cont
  }
  
  define i64 @id(i64 %0) {
  entry:
    ret i64 %0
  }
  
  define i64 @main() {
  entry:
    %0 = call i64 @rukaml_alloc_closure(i64 ptrtoint (i64 (i64)* @id to i64), i64 1)
    %1 = call i64 @rukaml_alloc_closure(i64 ptrtoint (i64 (i64, i64)* @fack to i64), i64 2)
    %2 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %1, i64 1, i64 %0)
    %3 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %2, i64 1, i64 4)
    ret i64 %3
  }

  $ clang-14 fack.ll ../../compiler/rukaml_stdlib.o -o fack.exe
  warning: overriding the module target triple with x86_64-pc-linux-gnu [-Woverride-module]
  1 warning generated.
  $ ./fack.exe
  [24]


  $ cat << EOF | ../../llvm/llvm_compiler.exe -o fack.ll
  > let rec fack n k =
  >   if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > let main = fack 5 (fun x -> x)
  > EOF
  After ANF transformation.
  let fresh_1 n k m =
    let temp4 = (n * m) in
      k temp4 
  let rec fack n k =
    let temp8 = (n = 1) in
      (if temp8
      then k 1 
      else let temp10 = (n - 1) in
             let temp11 = fack temp10  in
               let temp12 = fresh_1 n  in
                 let temp13 = temp12 k  in
                   temp11 temp13 )
  let fresh_2 x =
    x
  let main =
    let temp17 = fack 5  in
      temp17 fresh_2 
  let fresh_1 n k m =
    let temp4 = (n * m) in
      k temp4 
  let rec fack n k =
    let temp8 = (n = 1) in
      (if temp8
      then k 1 
      else let temp10 = (n - 1) in
             let temp11 = fack temp10  in
               let temp12 = fresh_1 n  in
                 let temp13 = temp12 k  in
                   temp11 temp13 )
  let fresh_2 x =
    x
  let main =
    let temp17 = fack 5  in
      temp17 fresh_2 
  $ clang-14 fack.ll ../../compiler/rukaml_stdlib.o -o fack.exe
  warning: overriding the module target triple with x86_64-pc-linux-gnu [-Woverride-module]
  1 warning generated.
  $ ./fack.exe
  [120]
