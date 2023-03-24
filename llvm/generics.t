  $ cat > test.ml <<-EOF
  > let fplus = fun f -> fun x -> fun y -> f x + f y
  > let square = fun x -> x * x
  > let plus = fplus square
  > let ret_zero = fun x -> 0
  > let main = ret_zero (trace_int (plus (get_int_arg 1) (get_int_arg 2)))
  > EOF
  $ ./llvm_compiler.exe test.ml -o test.o
  $ gcc runtime.c -c -o runtime.o
  $ gcc runtime.o test.o -o test.exe
  $ ./test.exe 0 0
  0
  $ ./test.exe 1 2
  5
