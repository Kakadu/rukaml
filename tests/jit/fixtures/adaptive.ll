declare i64 @rukaml_alloc_closure(i64, i64)
declare i64 @rukaml_applyN(i64, i64, ...)

define i64 @inc(i64 %0) {
  %r = add i64 %0, 1
  ret i64 %r
}

define i64 @dec(i64 %0) {
  %r = sub i64 %0, 1
  ret i64 %r
}

define i64 @drive(i64 %k, i64 %n, i64 %acc) {
entry:
  %c = icmp eq i64 %n, 0
  br i1 %c, label %done, label %rec

rec:
  %p = call i64 (i64, i64, ...) @rukaml_applyN(i64 %k, i64 1, i64 %n)
  %a = add i64 %acc, %p
  %m = sub i64 %n, 1
  %r = call i64 @drive(i64 %k, i64 %m, i64 %a)
  ret i64 %r

done:
  ret i64 %acc
}

define i64 @main() {
  %ci = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @inc to i64), i64 1)
  %cd = call i64 @rukaml_alloc_closure(i64 ptrtoint (ptr @dec to i64), i64 1)
  %a = call i64 @drive(i64 %ci, i64 30000, i64 0)
  %b = call i64 @drive(i64 %cd, i64 30000, i64 %a)
  ret i64 %b
}
