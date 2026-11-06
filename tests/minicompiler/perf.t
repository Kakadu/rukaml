

$ sed 's/(\*IMPORTANT\*);;//g' 1.ml > 2.ml

  $ /usr/bin/time -f "%U" ../../driver/driver.exe --target parsetree compiler.ml -o 1
  0.30
$ /usr/bin/time -f "%U" ../../driver/driver.exe --target parsetree 2.ml -o 2


$ /usr/bin/time -f "%U" ../../driver/driver.exe --target parsetree compiler.ml -o 1
