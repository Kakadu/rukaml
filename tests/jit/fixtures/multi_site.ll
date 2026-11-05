declare i64 @rukaml_alloc_closure(i64, i64)
declare i64 @rukaml_applyN(i64, i64, ...)

define i64 @add(i64 %0, i64 %1) {
  %r = add i64 %0, %1
  ret i64 %r
}

define i64 @mul(i64 %0, i64 %1) {
  %r = mul i64 %0, %1
  ret i64 %r
}

define i64 @main() {
  %ca = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @add to i64), i64 2)
  %pa = call i64 (i64, i64, ...) @rukaml_applyN(i64 %ca, i64 1, i64 3)
  %ra = call i64 (i64, i64, ...) @rukaml_applyN(i64 %pa, i64 1, i64 4)
  %cm = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @mul to i64), i64 2)
  %pm = call i64 (i64, i64, ...) @rukaml_applyN(i64 %cm, i64 1, i64 %ra)
  %rm = call i64 (i64, i64, ...) @rukaml_applyN(i64 %pm, i64 1, i64 5)
  ret i64 %rm
}
