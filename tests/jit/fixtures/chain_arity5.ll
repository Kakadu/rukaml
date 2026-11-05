declare i64 @rukaml_alloc_closure(i64, i64)
declare i64 @rukaml_applyN(i64, i64, ...)

define i64 @f5(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4) {
  %a = add i64 %0, %1
  %b = add i64 %a, %2
  %c = add i64 %b, %3
  %r = add i64 %c, %4
  ret i64 %r
}

define i64 @main() {
  %c = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @f5 to i64), i64 5)
  %p1 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %c, i64 1, i64 1)
  %p2 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %p1, i64 1, i64 2)
  %p3 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %p2, i64 1, i64 3)
  %p4 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %p3, i64 1, i64 4)
  %r = call i64 (i64, i64, ...) @rukaml_applyN(i64 %p4, i64 1, i64 32)
  ret i64 %r
}
