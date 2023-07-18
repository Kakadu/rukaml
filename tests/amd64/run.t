$ which minimlc
$ ls /home/kakadu/asp/miniml/_build/install/default/bin

# CPS Factorial
  $ cat << EOF | minimlc -dtypedtree -amd64 -amd64-v -
  > let rec fack n k =
  >  if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > 
  > let id2 x = x
  > let main = fack 6 id2
  > EOF
; generated code for amd64
  $ cat a.out
