  $ cat > sum.ml <<'EOF'
  > let rec sum n acc = if n = 0 then acc else sum (n - 1) (acc + n)
  > let main = sum 10 0
  > EOF

The exit code is main's value.

  $ ../../driver/driver.exe sum.ml --target jit
  [55]

Offline passes keep the result.

  $ ../../driver/driver.exe sum.ml --target jit --opt=devirt,o2
  [55]
  $ ../../driver/driver.exe sum.ml --target jit --opt devirt --opt o2 --opt tier0
  [55]
  $ ../../driver/driver.exe sum.ml --target jit --opt=tier0
  [55]

Profile, then specialize with that profile.

  $ ../../driver/driver.exe sum.ml --target jit --opt=profile --profile-file sum.prof
  [55]
  $ cat sum.prof
  1	main
  11	sum
  $ ../../driver/driver.exe sum.ml --target jit --opt=specialize --profile-file sum.prof
  [55]

The dump directory gets the input and each pass output in order.

  $ ../../driver/driver.exe sum.ml --target jit --opt=devirt,o2 --opt-dump dump
  [55]
  $ ls dump
  00-input.ll
  01-devirt.ll
  02-o2.ll

-o writes the IR after the pipeline.

  $ ../../driver/driver.exe sum.ml --target jit --opt=o2 -o out.ll
  [55]
  $ grep -c "^define" out.ll
  2

Errors.

  $ ../../driver/driver.exe sum.ml --target jit --opt=nope
  jit error: unknown JIT optimization pass: "nope"
  [1]
  $ ../../driver/driver.exe sum.ml --target jit --opt=specialize --profile-file missing.prof
  jit error: specialize: profile file missing.prof does not exist (run with --opt=profile first)
  [1]
  $ echo garbage > bad.prof
  $ ../../driver/driver.exe sum.ml --target jit --opt=specialize --profile-file bad.prof
  jit error: specialize: malformed profile line "garbage" in bad.prof
  [1]
  $ touch blocker
  $ ../../driver/driver.exe sum.ml --target jit --opt=o2 --opt-dump blocker/dump
  jit error: cannot create dump directory blocker/dump: blocker/dump: Not a directory
  [1]
