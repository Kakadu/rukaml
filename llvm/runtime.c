#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <stdbool.h>
#include <errno.h>
// #include <glib.h>

// GC

const int NON_GC_TYPE = -1;
const int CLOSURE_TYPE = 0;
const int VECTOR_TYPE = 1;


typedef struct
{
  void* pointer;
  void* info;
  int type;
  bool marked;
} gc_pointer;

typedef struct
{
  int size;
  int elem_size;
} vector_info;

typedef struct
{
  void *code;
  int32_t argsc;
  int32_t args_received;
  void *args[0];
} closure;


void track_pointer(gc_pointer* gcp) {
  return;
}

bool is_tracked_pointer(void* p) {

}

gc_pointer* gc_alloc(int size, int type, void* info) {
  gc_pointer* gcp = malloc(sizeof(gc_pointer));
  void* memory = (void*) malloc(size);

  gcp->pointer = memory;
  gcp->type = type;
  gcp->marked = false;

  track_pointer(gcp);

  return gcp;
}

void* gc_alloc_closure(int size) {
  gc_pointer* gcp = gc_alloc(size, CLOSURE_TYPE, NULL);
  return gcp->pointer;
}

void* gc_alloc_vector(int elem_size, int vector_size) {
  vector_info* info = malloc(sizeof(vector_info));
  info->size = vector_size;
  info->elem_size = elem_size;

  gc_pointer* gcp = gc_alloc(elem_size * vector_size, VECTOR_TYPE, info);

  return gcp->pointer;
}

void gc_free(gc_pointer* gcp) {
  return;
}

// IO function
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


// Closures
typedef void *(*fun0)(void);
typedef void *(*fun1)(void *);
typedef void *(*fun2)(void *, void *);
typedef void *(*fun3)(void *, void *, void *);
typedef void *(*fun4)(void *, void *, void *, void *);
typedef void *(*fun5)(void *, void *, void *, void *, void *);
typedef void *(*fun6)(void *, void *, void *, void *, void *, void *);
typedef void *(*fun7)(void *, void *, void *, void *, void *, void *, void *);

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

void *apply3(void *f, void *arg1, void *arg2, void *arg3)
{
  fun3 foo = (fun3)f;
  return foo(arg1, arg2, arg3);
}

void *apply4(void *f, void *arg1, void *arg2, void *arg3, void *arg4)
{
  fun4 foo = (fun4)f;
  return foo(arg1, arg2, arg3, arg4);
}

void *apply5(void *f, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5)
{
  fun5 foo = (fun5)f;
  return foo(arg1, arg2, arg3, arg4, arg5);
}

void *apply6(void *f, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6)
{
  fun6 foo = (fun6)f;
  return foo(arg1, arg2, arg3, arg4, arg5, arg6);
}

void *apply7(void *f, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7)
{
  fun7 foo = (fun7)f;
  return foo(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

closure *copy_closure(closure *src)
{
  size_t size = sizeof(closure) + sizeof(void *) * src->argsc;
  closure *dst = (closure *)gc_alloc_closure(size);
  return memcpy(dst, src, size);
}

void *alloc_closure(void *func, int32_t argsc)
{
  // printf("%s argc = %u\n", __func__, argsc);
  fflush(stdout);
  closure *ans = (closure *) gc_alloc_closure(sizeof(closure) + sizeof(void *) * argsc);
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
  // printf("\n\n");
  // printf("%s argc = %u, closure = %p\n", __func__, argc, f);
  fflush(stdout);

  va_list argp;
  va_start(argp, argc);
  closure *f_closure = copy_closure((closure *)f);
  // printf("f->arg_received = %u\n", f_closure->args_received);
  // printf("%d\n", __LINE__);
  assert(f_closure->args_received + argc <= f_closure->argsc);

  for (size_t i = 0; i < argc; i++)
  {
    // printf("%d\n", __LINE__);
    void *arg1 = va_arg(argp, void *);
    // printf("arg[%lu] = %p, ", i, arg1);
    // fflush(stdout);
    f_closure->args[f_closure->args_received++] = arg1;
  }
  // printf("\n");
  // printf("f->arg_received = %u, f->argc = %u\n",
  //        f_closure->args_received,
  //        f_closure->argsc);
  // fflush(stdout);
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
    case 3:
      return apply3(f_closure->code, f_closure->args[0], f_closure->args[1], f_closure->args[2]);
      break;
    case 4:
      return apply4(f_closure->code, f_closure->args[0], f_closure->args[1], f_closure->args[2], f_closure->args[3]);
      break;
    case 5:
      return apply5(f_closure->code, f_closure->args[0], f_closure->args[1], f_closure->args[2], f_closure->args[3], f_closure->args[4]);
      break;
    case 6:
      return apply6(f_closure->code, f_closure->args[0], f_closure->args[1], f_closure->args[2], f_closure->args[3], f_closure->args[4], f_closure->args[5]);
      break;
    case 7:
      return apply7(f_closure->code, f_closure->args[0], f_closure->args[1], f_closure->args[2], f_closure->args[3], f_closure->args[4], f_closure->args[5], f_closure->args[6]);
      break;
    default:
      printf("FUCK\n");
      assert(false);
    }
  }
  return f_closure;
}

// Main wrapper
extern int real_main(void);
char** arguments;

int get_int_arg(int n) {
  return atoi(arguments[n]);
}

int main(int argc, char** argv) {
  arguments = argv;
  int result = real_main();
  return result;
}