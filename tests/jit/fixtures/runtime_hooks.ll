declare void @rukaml_jit_hot(i64)
declare void @rukaml_prof_args(i64, i64, i64, i64, i64)
declare void @rukaml_prof_site(i64, ptr)
declare i64 @rukaml_alloc_closure(i64, i64)

define i64 @f(i64 %0, i64 %1) {
  %r = add i64 %0, %1
  ret i64 %r
}

define i64 @main() {
  call void @rukaml_jit_hot(i64 3)
  call void @rukaml_prof_args(i64 0, i64 7, i64 8, i64 9, i64 10)
  call void @rukaml_prof_args(i64 0, i64 7, i64 99, i64 9, i64 10)
  %cl = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @f to i64), i64 2)
  %p = inttoptr i64 %cl to ptr
  call void @rukaml_prof_site(i64 2, ptr %p)
  ret i64 5
}
