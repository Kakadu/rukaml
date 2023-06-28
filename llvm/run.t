  $ cat > test1.ml <<-EOF
  > let add = fun  x -> fun  y -> x + y in
  > let add1 = add 1 in
  > add1 13
  > EOF
  $ ./llvm_compiler.exe test1.ml
  : end_of_input
