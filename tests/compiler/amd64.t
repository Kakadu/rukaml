  $ chmod +w program.c
  $ echo 'int main() { return 0; }' > program.c
  $ ./amd64.exe program.c -o file.out --target parsetree | grep -v rukaml_print_int
  [fatal] file=rukaml_stdlib.c line=881 msg="unexpected null ptr"
  [1]

$ ./amd64.exe program.c -o file.out --target rv64
