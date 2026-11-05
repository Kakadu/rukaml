declare i64 @rukaml_alloc_closure(i64, i64)
declare i64 @rukaml_applyN(i64, i64, ...)

define i64 @f(i64 %0, i64 %1) {
  %r = add i64 %0, %1
  ret i64 %r
}

define i64 @main() {
  %cl = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @f to i64), i64 2)
  %p = call i64 (i64, i64, ...) @rukaml_applyN(i64 %cl, i64 1, i64 4)
  %r = call i64 (i64, i64, ...) @rukaml_applyN(i64 %p, i64 1, i64 7)
  ret i64 %r
}
