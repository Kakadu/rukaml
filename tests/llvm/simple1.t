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
  Fatal error: exception Failure("Toplevel name \"id\" not found")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from LLVM_impl.on_vb.(fun).gen_c in file "llvm/LLVM_impl.ml", line 102, characters 21-44
  Called from LLVM_impl.on_vb.(fun) in file "llvm/LLVM_impl.ml", line 234, characters 19-27
  Called from Stdlib__List.iter in file "list.ml", line 110, characters 12-15
  Called from LLVM_impl.codegen in file "llvm/LLVM_impl.ml", line 338, characters 2-64
  Called from Dune__exe__Llvm_compiler.ToLLVM.run in file "llvm/llvm_compiler.ml", line 47, characters 4-38
  Called from Dune__exe__Llvm_compiler in file "llvm/llvm_compiler.ml", line 68, characters 8-22
  [2]
  $ cat fack.ll | grep 'target datalayout' --invert-match
  cat: fack.ll: No such file or directory
  [1]
  $ clang-16 fack.ll ../../compiler/rukaml_stdlib.o -o fack.exe
  clang: error: no such file or directory: 'fack.ll'
  [1]
  $ ./fack.exe
  ./fack.exe: not found
  [127]
