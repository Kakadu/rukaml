Array literals call rukaml_alloc_array, which the JIT must resolve.

  $ cat > arr.ml <<'EOF'
  > let main =
  >   let r = [|1; 2; 3|] in
  >   7
  > EOF
  $ ../../driver/driver.exe arr.ml --target jit
  [7]
  $ ../../driver/driver.exe arr.ml --target jit --opt=online
  [7]
