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
  int G, H;
  #include <stdbool.h>
  int test(bool Cond) {
    int X;
    if (Cond) X = G;
    else      X = H;
    return X;
  }
  int main() { return test(1); }
  $ clang -S -emit-llvm -O0 -Xclang -disable-O0-optnone test.c
  $ cat test.ll | grep -E 'source_filename|target datalayout|ModuleID|target triple|llvm.module.flags|llvm.ident|attributes #0|![0-9] =|^$' --invert-match
  @G = dso_local global i32 0, align 4
  @H = dso_local global i32 0, align 4
  ; Function Attrs: noinline nounwind uwtable
  define dso_local i32 @test(i1 noundef zeroext %0) #0 {
    %2 = alloca i8, align 1
    %3 = alloca i32, align 4
    %4 = zext i1 %0 to i8
    store i8 %4, i8* %2, align 1
    %5 = load i8, i8* %2, align 1
    %6 = trunc i8 %5 to i1
    br i1 %6, label %7, label %9
  7:                                                ; preds = %1
    %8 = load i32, i32* @G, align 4
    store i32 %8, i32* %3, align 4
    br label %11
  9:                                                ; preds = %1
    %10 = load i32, i32* @H, align 4
    store i32 %10, i32* %3, align 4
    br label %11
  11:                                               ; preds = %9, %7
    %12 = load i32, i32* %3, align 4
    ret i32 %12
  }
  ; Function Attrs: noinline nounwind uwtable
  define dso_local i32 @main() #0 {
    %1 = alloca i32, align 4
    store i32 0, i32* %1, align 4
    %2 = call i32 @test(i1 noundef zeroext true)
    ret i32 %2
  }
  $ llvm-as < test.ll | opt -mem2reg | llvm-dis | grep -E 'source_filename|target datalayout|ModuleID|target triple|llvm.module.flags|llvm.ident|attributes #0|![0-9] =|^$' --invert-match
  @G = dso_local global i32 0, align 4
  @H = dso_local global i32 0, align 4
  ; Function Attrs: noinline nounwind uwtable
  define dso_local i32 @test(i1 noundef zeroext %0) #0 {
    %2 = zext i1 %0 to i8
    %3 = trunc i8 %2 to i1
    br i1 %3, label %4, label %6
  4:                                                ; preds = %1
    %5 = load i32, i32* @G, align 4
    br label %8
  6:                                                ; preds = %1
    %7 = load i32, i32* @H, align 4
    br label %8
  8:                                                ; preds = %6, %4
    %.0 = phi i32 [ %5, %4 ], [ %7, %6 ]
    ret i32 %.0
  }
  ; Function Attrs: noinline nounwind uwtable
  define dso_local i32 @main() #0 {
    %1 = call i32 @test(i1 noundef zeroext true)
    ret i32 %1
  }

https://www.cs.colostate.edu/~cs553/Assignments/PA2/PA2-writeup.html
CSE TODO
