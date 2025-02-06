  $ cat << EOF | ./REPL.exe -vb -
  > let get arr = arr.[0]
  > EOF
  $ cat << EOF | ./REPL.exe -long -v -
  > (f.[0]+1)
  > EOF
  $ cat << EOF | ./REPL.exe -long -v -
  > ((f).[0]+1)
  > EOF
  $ cat << EOF | ./REPL.exe -long -v -
  > (f).[0]+1
  > EOF
