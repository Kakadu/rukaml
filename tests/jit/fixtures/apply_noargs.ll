declare i64 @rukaml_applyN(i64, i64, ...)

define i64 @z(i64 %c) {
  %r = call i64 (i64, i64, ...) @rukaml_applyN(i64 %c, i64 0)
  ret i64 %r
}
