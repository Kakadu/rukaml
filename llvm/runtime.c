#include<stdio.h>
#include<stdlib.h>

extern int real_main(void);

void print_int(int x) {
  printf("%d\n", x);
}

void print_bool(int x) {
  if (x == 0) {
    printf("false\n");
  } else {
    printf("true\n");
  }
}

int trace_int(int x) {
  print_int(x);
  return x;
}

int trace_bool(int x) {
  print_bool(x);
  return x;
}

int* resolve_pointer(int* p) {

}

char** arguments;

int get_int_arg(int n) {
  return atoi(arguments[n]);
}

int main(int argc, char** argv) {
  arguments = argv;
  return real_main();
}