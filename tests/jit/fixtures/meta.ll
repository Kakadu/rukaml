define i64 @f(i64 %0, i64 %1) {
  %r = add i64 %0, %1
  ret i64 %r
}

define i64 @g(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4) {
  ret i64 %0
}

define i64 @h() {
  ret i64 1
}

define i64 @main() {
  %r = call i64 @f(i64 1, i64 2)
  ret i64 %r
}
