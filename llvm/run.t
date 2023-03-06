  $ cat > test.ml <<-EOF
  > let square = fun x -> x * x
  > let sum_squares = fun x -> fun y -> square x + square y
  > let ret_zero = fun x -> x - x
  > let main = ret_zero (trace_int (sum_squares (get_int_arg 1) (get_int_arg 2)))
  > EOF
  $ ./llvm_compiler.exe test.ml -o test.o
  $ gcc runtime.c -c -o runtime.o
  $ gcc runtime.o test.o -o test.exe
  $ ./test.exe 1 2
  5
  $ ./test.exe 3 4
  25
