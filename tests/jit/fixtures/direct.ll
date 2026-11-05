define i64 @f(i64 %0, i64 %1) {
  %r = add i64 %0, %1
  ret i64 %r
}

define i64 @h(i64 %a, i64 %b) {
  %r = call i64 @f(i64 %a, i64 %b)
  ret i64 %r
}

define i64 @main() {
  %r = call i64 @f(i64 1, i64 2)
  ret i64 %r
}
