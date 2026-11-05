define i64 @x() {
  ret i64 12
}

define i64 @main() {
  %r = call i64 @x()
  %s = add i64 %r, 30
  ret i64 %s
}
