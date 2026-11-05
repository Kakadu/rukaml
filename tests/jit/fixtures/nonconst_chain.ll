declare i64 @rukaml_alloc_closure(i64, i64)
declare i64 @rukaml_applyN(i64, i64, ...)

define i64 @add(i64 %0, i64 %1) {
  %r = add i64 %0, %1
  ret i64 %r
}

define i64 @step(i64 %a, i64 %b) {
  %cl = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @add to i64), i64 2)
  %p1 = call i64 (i64, i64, ...) @rukaml_applyN(i64 %cl, i64 1, i64 %a)
  %r = call i64 (i64, i64, ...) @rukaml_applyN(i64 %p1, i64 1, i64 %b)
  ret i64 %r
}

define i64 @main() {
  %res = call i64 @step(i64 19, i64 23)
  ret i64 %res
}
