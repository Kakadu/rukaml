

# CPS Factorial
  $ cat << EOF | ./compiler.exe -o fack.ll #-vllvm
  > let fresh_1 m k n = k (n * m)
  > let rec fack k n =
  >   if n=1
  >   then k 1
  >   else fack (fresh_1 n k) (n-1)
  > let id x = x
  > let main = fack id 4
  > EOF
  After ANF transformation.
  let fresh_1 m k n =
    let temp1 = (n * m) in
      k temp1 
  let rec fack k n =
    (if (n = 1)
    then k 1 
    else let temp5 = fresh_1 n  in
           let temp6 = temp5 k  in
             let temp7 = fack temp6  in
               let temp8 = (n - 1) in
                 temp7 temp8 )
  let id x =
    x
  let main =
    let temp11 = fack id  in
      temp11 4 
  let fresh_1 m k n =
    let temp1 = (n * m) in
      k temp1 
  let rec fack k n =
    (if (n = 1)
    then k 1 
    else let temp5 = fresh_1 n  in
           let temp6 = temp5 k  in
             let temp7 = fack temp6  in
               let temp8 = (n - 1) in
                 temp7 temp8 )
  let id x =
    x
  let main =
    let temp11 = fack id  in
      temp11 4 

  $ cat fack.ll | grep 'target datalayout' --invert-match
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "x86_64-pc-linux-gnu"
  
  declare void @myputc(i64)
  
  declare i64 @rukaml_applyN(i64, i64, ...)
  
  declare i64 @rukaml_alloc_closure(i64, i64)
  
  declare i64 @rukaml_alloc_pair(i64, i64)
  
  declare i64 @rukaml_field(i64, i64)
  
  define i64 @fresh_1(i64 %0, i64 %1, i64 %2) {
  entry:
    %3 = mul i64 %2, %0
    %4 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %1, i64 1, i64 %3)
    ret i64 %4
  }
  
  define i64 @fack(i64 %0, i64 %1) {
  entry:
    %ifcond = icmp eq i64 %1, 1
    br i1 %ifcond, label %fack_then, label %fack_else
  
  fack_cont:                                        ; preds = %fack_else, %fack_then
    %phi_result = phi i64 [ %9, %fack_then ], [ %8, %fack_else ]
    ret i64 %phi_result
  
  fack_else:                                        ; preds = %entry
    %2 = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @fresh_1 to i64), i64 3)
    %3 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %2, i64 1, i64 %1)
    %4 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %3, i64 1, i64 %0)
    %5 = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @fack to i64), i64 2)
    %6 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %5, i64 1, i64 %4)
    %7 = sub i64 %1, 1
    %8 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %6, i64 1, i64 %7)
    br label %fack_cont
  
  fack_then:                                        ; preds = %entry
    %9 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %0, i64 1, i64 1)
    br label %fack_cont
  }
  
  define i64 @id(i64 %0) {
  entry:
    ret i64 %0
  }
  
  define i64 @main() {
  entry:
    %0 = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @id to i64), i64 1)
    %1 = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @fack to i64), i64 2)
    %2 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %1, i64 1, i64 %0)
    %3 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %2, i64 1, i64 4)
    ret i64 %3
  }

  $ clang-16 fack.ll ../../middle/rukaml_stdlib.o -o fack.exe
  $ ./fack.exe
  [24]


  $ cat << EOF | ./compiler.exe -o fack.ll
  > let rec fack n k =
  >   if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > let main = fack 5 (fun x -> x)
  > EOF
  After ANF transformation.
  let fresh_1 n k m =
    let temp1 = (n * m) in
      k temp1 
  let rec fack n k =
    (if (n = 1)
    then k 1 
    else let temp5 = (n - 1) in
           let temp6 = fack temp5  in
             let temp7 = fresh_1 n  in
               let temp8 = temp7 k  in
                 temp6 temp8 )
  let fresh_2 x =
    x
  let main =
    let temp11 = fack 5  in
      temp11 fresh_2 
  let fresh_1 n k m =
    let temp1 = (n * m) in
      k temp1 
  let rec fack n k =
    (if (n = 1)
    then k 1 
    else let temp5 = (n - 1) in
           let temp6 = fack temp5  in
             let temp7 = fresh_1 n  in
               let temp8 = temp7 k  in
                 temp6 temp8 )
  let fresh_2 x =
    x
  let main =
    let temp11 = fack 5  in
      temp11 fresh_2 
  $ clang-16 fack.ll ../../middle/rukaml_stdlib.o -o fack.exe
  $ ./fack.exe
  [120]
