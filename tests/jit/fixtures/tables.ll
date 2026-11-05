@rukaml.slots = global [1 x ptr] zeroinitializer
@rukaml.cnt = global [1 x i64] zeroinitializer
@rukaml.miss = global [1 x i64] zeroinitializer

define i64 @main() {
  store i64 5, ptr @rukaml.cnt
  ret i64 0
}

define i64 @f(i64 %0) {
  ret i64 %0
}
