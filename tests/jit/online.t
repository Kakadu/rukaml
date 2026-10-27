  $ cat > sum.ml <<'EOF'
  > let rec sum n acc = if n = 0 then acc else sum (n - 1) (acc + n)
  > let main = sum 1000 0
  > EOF

Online tier-up returns the same value: 500500 mod 256.

  $ ../../driver/driver.exe sum.ml --target jit --opt=online
  [20]

Other passes are ignored under online.

  $ ../../driver/driver.exe sum.ml --target jit --opt=online --opt=nope --opt=o2
  [20]

The dump has the input and the tier0 module; delta modules depend on timing.

  $ ../../driver/driver.exe sum.ml --target jit --opt=online --opt-dump dump
  [20]
  $ ls dump | grep -E '^(00-input|01-tier0)\.ll$'
  00-input.ll
  01-tier0.ll

-o writes the master IR, not the tier0 one.

  $ ../../driver/driver.exe sum.ml --target jit --opt=online -o out.ll
  [20]
  $ grep -c "^define" out.ll
  2
  $ grep -q "rukaml_jit_hot" out.ll || echo no-tier0-hooks
  no-tier0-hooks
