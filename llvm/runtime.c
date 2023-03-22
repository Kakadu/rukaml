#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <stdbool.h>
#include <errno.h>

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

typedef void *(*fun0)(void);
typedef void *(*fun1)(void *);
typedef void *(*fun2)(void *, void *);

void *apply0(void *f)
{
  fun0 foo = (fun0)f;
  return foo();
}

void *apply1(void *f, void *arg1)
{
  fun1 foo = (fun1)f;
  return foo(arg1);
}

void *apply2(void *f, void *arg1, void *arg2)
{
  fun2 foo = (fun2)f;
  return foo(arg1, arg2);
}
typedef struct
{
  void *code;
  int32_t argsc; // TODO(Kakadu): uint32_t or byte ???
  int32_t args_received;
  void *args[0];
} closure;

closure *copy_closure(closure *src)
{
  size_t size = sizeof(closure) + sizeof(void *) * src->argsc;
  closure *dst = (closure *)malloc(size);
  return memcpy(dst, src, size);
}

void *alloc_closure(void *func, int32_t argsc)
{
  // printf("%s argc = %u\n", __func__, argsc);
  fflush(stdout);
  closure *ans = (closure *)malloc(sizeof(closure) + sizeof(void *) * argsc);
  //  { .code = func, .argsc = argsc }
  ans->code = func;
  ans->argsc = argsc;
  ans->args_received = 0;
  memset(ans->args, 0, argsc * sizeof(void *));
  return ans;
}

void *applyN(void *f, int32_t argc, ...)
{
  setbuf(stdout, NULL);
  printf("%s argc = %u, closure = %p\n", __func__, argc, f);
  fflush(stdout);

  va_list argp;
  va_start(argp, argc);
  closure *f_closure = copy_closure((closure *)f);
  printf("f->arg_received = %u\n", f_closure->args_received);
  //printf("%d\n", __LINE__);
  assert(f_closure->args_received + argc <= f_closure->argsc);

  for (size_t i = 0; i < argc; i++)
  {
    // printf("%d\n", __LINE__);
    void *arg1 = va_arg(argp, void *);
    // printf("arg[%lu] = %p, ", i, arg1);
    fflush(stdout);
    f_closure->args[f_closure->args_received++] = arg1;
  }
  /* printf("\n");
  printf("f->arg_received = %u, f->argc = %u\n",
         f_closure->args_received,
         f_closure->argsc);
  fflush(stdout); */
  va_end(argp);
  if (f_closure->argsc == f_closure->args_received)
  {
    switch (f_closure->argsc)
    {
    case 0:
      return apply0(f_closure->code);
      break;
    case 1:
      return apply1(f_closure->code, f_closure->args[0]);
      break;
    case 2:
      return apply2(f_closure->code, f_closure->args[0], f_closure->args[1]);
      break;
    default:
      printf("FUCK\n");
      assert(false);
    }
  }
  return f_closure;
}

extern int real_main(void);
char** arguments;

int get_int_arg(int n) {
  return atoi(arguments[n]);
}

int main(int argc, char** argv) {
  arguments = argv;
  return real_main();
}