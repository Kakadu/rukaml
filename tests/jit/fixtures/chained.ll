declare i64 @rukaml_alloc_closure(i64, i64)
declare i64 @rukaml_applyN(i64, i64, ...)

define i64 @f(i64 %0, i64 %1) {
  %r = add i64 %0, %1
  ret i64 %r
}

define i64 @main() {
  %c1 = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @f to i64), i64 2)
  %p1 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %c1, i64 1, i64 10)
  %r1 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %p1, i64 1, i64 5)
  %c2 = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @f to i64), i64 2)
  %p2 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %c2, i64 1, i64 100)
  %r2 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %p2, i64 1, i64 %r1)
  ret i64 %r2
}
