declare i64 @rukaml_alloc_closure(i64, i64)
declare i64 @rukaml_applyN(i64, i64, ...)

define i64 @inc(i64 %0) {
  %r = add i64 %0, 5
  ret i64 %r
}

define i64 @main() {
  %c = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @inc to i64), i64 1)
  %r = call i64 (i64, i64, ...) @rukaml_applyN(i64 %c, i64 1, i64 37)
  ret i64 %r
}
