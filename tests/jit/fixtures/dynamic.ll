declare i64 @rukaml_applyN(i64, i64, ...)

define i64 @g(i64 %c) {
  %r = call i64 (i64, i64, ...) @rukaml_applyN(i64 %c, i64 2, i64 1, i64 2)
  ret i64 %r
}
