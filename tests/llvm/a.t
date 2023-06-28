$ ls  ../..
$ ls  ../../llvm
$ ../../compiler.exe -help

# CPS Factorial
  $ cat << EOF | ../../llvm/llvm_compiler.exe
  > let rec fack n k =
  >   if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
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
             let temp11 = fack temp10  in
               let temp12 = fresh_1 n  in
                 let temp13 = temp12 k  in
                   temp11 temp13 )
  let fresh_1 m k n =
    let temp4 = (n * m) in
      k temp4 
  let rec fack k n =
    let temp8 = (n = 1) in
      (if temp8
      then k 1 
      else let temp10 = (n - 1) in
             let temp11 = fack temp10  in
               let temp12 = fresh_1 n  in
                 let temp13 = temp12 k  in
                   temp11 temp13 )
