# mem2reg demo
  $ cat << EOF > test.c
  > int G, H;
  > #include <stdbool.h>
  > int test(bool Cond) {
  >   int X;
  >   if (Cond) X = G;
  >   else      X = H;
  >   return X;
  > }
  > int main() { return test(1); }
  > EOF
  $ cat test.c
  $ clang -S -emit-llvm -O0 -Xclang -disable-O0-optnone test.c
  $ cat test.ll | grep -E 'source_filename|target datalayout|ModuleID|target triple|llvm.module.flags|llvm.ident|attributes #0|![0-9] =|^$' --invert-match
  $ llvm-as < test.ll | opt -mem2reg | llvm-dis | grep -E 'source_filename|target datalayout|ModuleID|target triple|llvm.module.flags|llvm.ident|attributes #0|![0-9] =|^$' --invert-match

https://www.cs.colostate.edu/~cs553/Assignments/PA2/PA2-writeup.html
CSE TODO
