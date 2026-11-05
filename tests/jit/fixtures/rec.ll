declare i64 @rukaml_alloc_closure(i64, i64)
declare i64 @rukaml_applyN(i64, i64, ...)

define i64 @f(i64 %0, i64 %1) {
entry:
  %c = icmp eq i64 %0, 0
  br i1 %c, label %done, label %rec

rec:
  %n = sub i64 %0, 1
  %a = add i64 %1, %0
  %r = call i64 @f(i64 %n, i64 %a)
  ret i64 %r

done:
  ret i64 %1
}

define i64 @main() {
  %r = call i64 @f(i64 100, i64 0)
  ret i64 %r
}
