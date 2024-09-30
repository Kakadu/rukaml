$ ls ../..
  $ cat << EOF | ../../compiler/cps_demo.exe -
  > let app f x = f x
  > let sum f x y = f x + f y
  > EOF
