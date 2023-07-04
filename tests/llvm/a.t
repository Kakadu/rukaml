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
  >   if n=1
  >     then k 1
  >     else let u = n-1 in 
  >          let v = fresh_1 n k in 
  >          fack v u
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
             let u = temp10 in
               let temp11 = fresh_1 n  in
                 let temp12 = temp11 k  in
                   let v = temp12 in
                     let temp13 = fack v  in
                       temp13 u )
  let fresh_1 n k m =
    let temp4 = (n * m) in
      k temp4 
  let rec fack n k =
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
  $ cat fac.ll | grep 'target datalayout' --invert-match
  ; ModuleID = 'main'
  source_filename = "main"
  
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
  
  define i64 @fack(i64 %0, i64 %1) {
  entry:
    call void @myputc(i64 48)
    %2 = icmp eq i64 %0, 1
    %name = zext i1 %2 to i64
    %ifcond = icmp ne i64 %name, 0
    br i1 %ifcond, label %fack_then, label %fack_else
  
  fack_cont:                                        ; preds = %fack_else, %fack_then
    %phi_result = phi i64* [ %9, %fack_then ], [ %7, %fack_else ]
    ret i64 0
  
  fack_else:                                        ; preds = %entry
    %3 = sub i64 %0, 1
    %4 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* bitcast (i64 (i64, i64, i64)* @fresh_1 to i64*), i64 0)
    %5 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* %4, i64 1, i64 %1)
    %6 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* bitcast (i64 (i64, i64)* @fack to i64*), i64 0)
    %7 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* %6, i64 1, i64 %3)
    br label %fack_cont
  
  fack_then:                                        ; preds = %entry
    %8 = inttoptr i64 %1 to i64*
    %9 = call i64* (i64*, i64, ...) @rukaml_applyN(i64* %8, i64 1, i64 1)
    br label %fack_cont
  }
