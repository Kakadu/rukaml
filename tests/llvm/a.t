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
  After ANF transformation.
  let fresh_1 n k m =
    let temp4 = (n * m) in
      k temp4 
  let temp n =
    (n = 1)
  let rec fack n k =
    let temp10 = temp n  in
      (if temp10
      then k 1 
      else let temp12 = fresh_1 n  in
             let temp13 = temp12 k  in
               let temp14 = fack temp13  in
                 let temp15 = (n - 1) in
                   temp14 temp15 )
  let fresh_1 n k m =
    let temp4 = (n * m) in
      k temp4 
  let temp n =
    (n = 1)
  let rec fack n k =
    let temp10 = temp n  in
      (if temp10
      then k 1 
      else let temp12 = fresh_1 n  in
             let temp13 = temp12 k  in
               let temp14 = fack temp13  in
                 let temp15 = (n - 1) in
                   temp14 temp15 )
  vb fresh_1
    formal parameter 0: i64 %0
    formal parameter 1: i64 %1
    formal parameter 2: i64 %2
  vb temp
    formal parameter 0: i64 %0
  vb fack
    formal parameter 0: i64 %0
    formal parameter 1: i64 %1
    arg 0: i64 %0
  LLVM_impl.on_vb.(fun).gen_c 54. temp n 
  LLVM_impl.on_vb.(fun).gen_c 56. applyN  =  declare i64* @rukaml_applyN(i64*, i64, ...)
  
  LLVM_impl.on_vb.(fun).gen_c 60. ptr  =  i64* bitcast (i64 (i64)* @temp to i64*)
    arg 0: i64 %0
  LLVM_impl.on_vb.(fun).gen_c 54. fresh_1 n 
  LLVM_impl.on_vb.(fun).gen_c 56. applyN  =  declare i64* @rukaml_applyN(i64*, i64, ...)
  
  LLVM_impl.on_vb.(fun).gen_c 60. ptr  =  i64* bitcast (i64 (i64, i64, i64)* @fresh_1 to i64*)
    arg 0:   %5 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* %4, i64 1, i64 %1)
  LLVM_impl.on_vb.(fun).gen_c 54. fack temp13 
  LLVM_impl.on_vb.(fun).gen_c 56. applyN  =  declare i64* @rukaml_applyN(i64*, i64, ...)
  
  LLVM_impl.on_vb.(fun).gen_c 60. ptr  =  i64* bitcast (i64 (i64, i64)* @fack to i64*)
  $ cat fac.ll
  ; ModuleID = 'main'
  source_filename = "main"
  target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
  
  declare void @myputc(i64)
  
  declare i64* @rukaml_applyN(i64*, i64, ...)
  
  define i64 @main() {
  entry:
    call void @myputc(i64 48)
    ret i64 0
  }
  
  define i64 @fresh_1(i64 %0, i64 %1, i64 %2) {
  entry:
    call void @myputc(i64 48)
    %3 = mul i64 %0, %2
    %4 = inttoptr i64 %1 to i64*
    %5 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* %4, i64 1, i64 %3)
    ret i64 0
  }
  
  define i64 @temp(i64 %0) {
  entry:
    call void @myputc(i64 48)
    %1 = icmp eq i64 %0, 1
    ret i64 0
  }
  
  define i64 @fack(i64 %0, i64 %1) {
  entry:
    call void @myputc(i64 48)
    %2 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* bitcast (i64 (i64)* @temp to i64*), i64 0)
    %3 = ptrtoint i64* %2 to i64
    %ifcond = icmp ne i64 %3, 0
    br i1 %ifcond, label %fack_then, label %fack_else
  
  fack_cont:                                        ; preds = %fack_else, %fack_then
    %phi_result = phi i64* [ %10, %fack_then ], [ %8, %fack_else ]
    ret i64 0
  
  fack_else:                                        ; preds = %entry
    %4 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* bitcast (i64 (i64, i64, i64)* @fresh_1 to i64*), i64 0)
    %5 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* %4, i64 1, i64 %1)
    %6 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* bitcast (i64 (i64, i64)* @fack to i64*), i64 0)
    %7 = sub i64 %0, 1
    %8 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* %6, i64 1, i64 %7)
    br label %fack_cont
  
  fack_then:                                        ; preds = %entry
    %9 = inttoptr i64 %1 to i64*
    %10 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* %9, i64 1, i64 1)
    br label %fack_cont
  }
