  $ run () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

  $ ../../driver/driver.exe --print-targets
  $ run < mutual2.ml
