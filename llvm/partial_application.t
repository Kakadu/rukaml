  $ cat > test.ml <<-EOF
  > let plus = fun x -> fun y -> x + y
  > let plus5 = plus 5
  > let ret_zero = fun x -> x - x
  > let main = ret_zero (trace_int (plus5 (get_int_arg 1)))
  > EOF
  $ ./llvm_compiler.exe test.ml -o test.o
  $ gcc runtime.c -c -o runtime.o
  $ gcc runtime.o test.o -o test.exe
  $ ./test.exe 0
  5
  $ ./test.exe 1
  6

  $ cat > test.ml <<-EOF
  > let plus = fun x -> fun y -> fun z -> x + y + z
  > let plus5 = plus 5
  > let plus10 = plus5 5
  > let ret_zero = fun x -> x - x
  > let main = ret_zero (trace_int (plus10 (get_int_arg 1)))
  > EOF
  $ ./llvm_compiler.exe test.ml -o test.o
  $ gcc runtime.c -c -o runtime.o
  $ gcc runtime.o test.o -o test.exe
  $ ./test.exe 0
  10
  $ ./test.exe 1
  11

  $ cat > test.ml <<-EOF
  > let plus = fun x -> fun y -> fun z -> fun m -> x + y + z + m
  > let plus5 = plus 5
  > let plus10 = plus5 5
  > let plus15 = plus10 5
  > let ret_zero = fun x -> x - x
  > let main = ret_zero (trace_int (plus15 (get_int_arg 1)))
  > EOF
  $ ./llvm_compiler.exe test.ml -o test.o
  $ gcc runtime.c -c -o runtime.o
  $ gcc runtime.o test.o -o test.exe
  $ ./test.exe 0
  15
  $ ./test.exe 1
  16

  $ cat > test.ml <<-EOF
  > let plus = fun x -> fun y -> fun z -> x + y + z
  > let plus5 = plus 5
  > let ret_zero = fun x -> x - x
  > let main = ret_zero (trace_int (plus5 (get_int_arg 1) (get_int_arg 2)))
  > EOF
  $ ./llvm_compiler.exe test.ml -o test.o
  $ gcc runtime.c -c -o runtime.o
  $ gcc runtime.o test.o -o test.exe
  $ ./test.exe 0 2
  7
  $ ./test.exe 3 4
  12
