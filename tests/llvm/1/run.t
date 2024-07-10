# factorial transalted to SSA
  $ cat << EOF > fac.c
  > static long fac(long n) {
  >   if (n<=1) return 1;
  >   else return n * fac(n-1);
  > }
  > int main() { return fac(5); }
  > EOF
  $ cat fac.c
  static long fac(long n) {
    if (n<=1) return 1;
    else return n * fac(n-1);
  }
  int main() { return fac(5); }
  $ clang-16 -S -emit-llvm -O0 fac.c
  $ cat fac.ll | grep -E 'source_filename|target datalayout|ModuleID|target triple|llvm.module.flags|llvm.ident|attributes #0|![0-9] =' --invert-match
  
  ; Function Attrs: noinline nounwind optnone uwtable
  define dso_local i32 @main() #0 {
    %1 = alloca i32, align 4
    store i32 0, ptr %1, align 4
    %2 = call i64 @fac(i64 noundef 5)
    %3 = trunc i64 %2 to i32
    ret i32 %3
  }
  
  ; Function Attrs: noinline nounwind optnone uwtable
  define internal i64 @fac(i64 noundef %0) #0 {
    %2 = alloca i64, align 8
    %3 = alloca i64, align 8
    store i64 %0, ptr %3, align 8
    %4 = load i64, ptr %3, align 8
    %5 = icmp sle i64 %4, 1
    br i1 %5, label %6, label %7
  
  6:                                                ; preds = %1
    store i64 1, ptr %2, align 8
    br label %13
  
  7:                                                ; preds = %1
    %8 = load i64, ptr %3, align 8
    %9 = load i64, ptr %3, align 8
    %10 = sub nsw i64 %9, 1
    %11 = call i64 @fac(i64 noundef %10)
    %12 = mul nsw i64 %8, %11
    store i64 %12, ptr %2, align 8
    br label %13
  
  13:                                               ; preds = %7, %6
    %14 = load i64, ptr %2, align 8
    ret i64 %14
  }
  
  
  




  $ clang-16 -S -emit-llvm -O1 fac.c
  $ cat fac.ll  | grep -E 'target datalayout|ModuleID|target triple|llvm.module.flags|llvm.ident|attributes #0|![0-9] =' --invert-match
  source_filename = "fac.c"
  
  ; Function Attrs: nofree nosync nounwind memory(none) uwtable
  define dso_local i32 @main() local_unnamed_addr #0 {
    %1 = tail call fastcc i64 @fac(i64 noundef 5)
    %2 = trunc i64 %1 to i32
    ret i32 %2
  }
  
  ; Function Attrs: nofree nosync nounwind memory(none) uwtable
  define internal fastcc i64 @fac(i64 noundef %0) unnamed_addr #0 {
    br label %2
  
  2:                                                ; preds = %6, %1
    %3 = phi i64 [ 1, %1 ], [ %8, %6 ]
    %4 = phi i64 [ %0, %1 ], [ %7, %6 ]
    %5 = icmp slt i64 %4, 2
    br i1 %5, label %9, label %6
  
  6:                                                ; preds = %2
    %7 = add nsw i64 %4, -1
    %8 = mul nsw i64 %3, %4
    br label %2
  
  9:                                                ; preds = %2
    %10 = mul nsw i64 %3, 1
    ret i64 %10
  }
  
  
  



