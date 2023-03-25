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

  $ cat > test.ml <<-EOF
  > let fac = fun x -> if (x = 0) then 1 else (x * fac (x - 1))
  > let ret_zero = fun x -> 0
  > let main = ret_zero (print_int (fac (get_int_arg 1)))
  > EOF
  $ ./llvm_compiler.exe test.ml -o test.o
  $ gcc runtime.c -c -o runtime.o
  $ gcc runtime.o test.o -o test.exe
  $ ./test.exe 1
  1
  $ ./test.exe 3
  6

  $ cat > test.ml <<-EOF
  > let mul_cps = fun x -> fun y -> fun f -> f (x * y)
  > let sum_cps = fun x -> fun y -> fun f -> f (x + y)
  > let id = fun x -> x
  > let sum_squares = fun x -> fun y -> mul_cps x x (fun sx -> mul_cps y y (fun sy -> sum_cps sx sy id))
  > let ret_zero = fun x -> 0
  > let main = ret_zero (print_int (sum_squares (get_int_arg 1) (get_int_arg 2)))
  > EOF
  $ ./llvm_compiler.exe test.ml -o test.o
  $ gcc runtime.c -c -o runtime.o
  $ gcc runtime.o test.o -o test.exe
  $ ./test.exe 1 1
  2
  $ ./test.exe 3 4
  25

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
