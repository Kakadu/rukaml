#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <stdbool.h>
#include <errno.h>

/* #define clean_errno() (errno == 0 ? "None" : strerror(errno))
#define log_error(M, ...) fprintf(stderr, "[ERROR] (%s:%d: errno: %s) " M "\n", __FILE__, __LINE__, clean_errno(), ##__VA_ARGS__)
#define assertf(A, M, ...)       \
  if (!(A))                      \
  {                              \
    log_error(M, ##__VA_ARGS__); \
    fflush(stderr);              \
    assert(A);                   \
  } */

#define DEBUG
// #undef DEBUG

void rukaml_print_int(int x)
{
  printf("%d\n", x);
  fflush(stdout);
}

typedef void *(*fun0)(void);
typedef void *(*fun1)(void *);
typedef void *(*fun2)(void *, void *);
typedef void *(*fun3)(void *, void *, void *);
typedef void *(*fun7)(void *, void *, void *, void *, void *, void *, void *);
typedef void *(*fun8)(void *, void *, void *, void *, void *, void *, void *, void *);
typedef void *(*fun9)(void *, void *, void *, void *, void *, void *, void *, void *, void *);

void *rukaml_apply0(void *f)
{
  // TODO: I'm not sure that zero-argument call is needed
  fun0 foo = (fun0)f;
  return foo();
}

// NOTE: Below we pass first 6 arguments as zeros, because they go to the registers.
// Others will go on stack
void *rukaml_apply1(void *f, void *arg1)
{
#ifdef DEBUG
  printf("%s f = %" PRIx64 ", arg = %" PRIx64 "\n", __func__, f, arg1);
#endif
  fun7 foo = (fun7)f;
  return foo(0, 0, 0, 0, 0, 0, arg1);
}

void *rukaml_apply2(void *f, void *arg1, void *arg2)
{
  fun8 foo = (fun8)f;
#ifdef DEBUG
  printf("call %s with code ptr = %" PRIx64 "\n", __func__, (uint64_t)f);
#endif
  return foo(0, 0, 0, 0, 0, 0, arg1, arg2);
}

void *rukaml_apply3(void *f, void *arg1, void *arg2, void *arg3)
{
  fun9 foo = (fun9)f;
  return foo(0, 0, 0, 0, 0, 0, arg1, arg2, arg3);
}

typedef struct
{
  void *code;
  int64_t argsc; // TODO(Kakadu): uint32_t or byte ???
  int64_t args_received;
  void *args[0];
} rukaml_closure;

rukaml_closure *copy_closure(rukaml_closure *src)
{
  size_t size = sizeof(rukaml_closure) + sizeof(void *) * src->argsc;
  rukaml_closure *dst = (rukaml_closure *)malloc(size);
#ifdef DEBUG
  printf("%s    %p    ~~>     %p\n", __func__, src, dst);
  fflush(stdout);
#endif
  return memcpy(dst, src, size);
}

void *rukaml_alloc_pair(void *l, void *r)
{
  void **rez = malloc(3 * sizeof(void *));
  (rez)[0] = 0; // tag
  (rez)[1] = l;
  (rez)[2] = r;
  return rez + 1;
}

void *rukaml_field(int n, void **r)
{
  return r[n];
}

int64_t myadd(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t a, int64_t b)
{
  printf("a = %ld, b = %ld\n", a, b);
  return a * b;
}

void *rukaml_alloc_closure(void *func, int32_t argsc)
{
  rukaml_closure *ans = (rukaml_closure *)malloc(sizeof(rukaml_closure) + sizeof(void *) * argsc);
  //  { .code = func, .argsc = argsc }
  ans->code = func;
  // ans->code = &myadd;
#ifdef DEBUG
  printf("store code ptr = %" PRIx64 "\n", (uint64_t)(ans->code));
#endif
  ans->argsc = argsc;
  ans->args_received = 0;
  memset(ans->args, 0, argsc * sizeof(void *));
#ifdef DEBUG
  printf("%s argc = %u,   %" PRIx64 "\n\n", __func__, argsc, (uint64_t)ans);
  fflush(stdout);
#endif
  return ans;
}

#include <unistd.h>

void *rukaml_applyN(void *f, int64_t argc, ...)
{

#ifdef DEBUG
  write(STDERR_FILENO, "HERE\n", 5);
  printf("%s argc = %lu, closure = %" PRIx64 "\n\n", __func__, argc, (uint64_t)f);
  printf("\tsaved code ptr = %" PRIx64 "\n", (uint64_t)(((rukaml_closure *)f)->code));
#endif
  va_list argp;
  va_start(argp, argc);
  rukaml_closure *f_closure = copy_closure((rukaml_closure *)f);
  // rukaml_closure *f_closure = f;
#ifdef DEBUG
  printf("\nf->arg_received = %lu, f->argc = %lu\n",
         f_closure->args_received,
         f_closure->argsc);
  fflush(stdout);
#endif
  // printf("f->arg_received = %u\n", f_closure->args_received);
  //  printf("%d\n", __LINE__);
  assert(f_closure->args_received + argc <= f_closure->argsc);

  for (size_t i = 0; i < argc; i++)
  {
    // printf("%d\n", __LINE__);
    void *arg1 = va_arg(argp, void *);
    // printf("arg[%lu] = %p, ", i, arg1);
    fflush(stdout);
    f_closure->args[f_closure->args_received++] = arg1;
  }
#ifdef DEBUG
  printf("\nf->arg_received = %lu, f->argc = %lu\n",
         f_closure->args_received,
         f_closure->argsc);
  fflush(stdout);
#endif
  va_end(argp);
  if (f_closure->argsc == f_closure->args_received)
  {
    switch (f_closure->argsc)
    {
    case 0:
      return rukaml_apply0(f_closure->code);
      break;
    case 1:
      return rukaml_apply1(f_closure->code, f_closure->args[0]);
      break;
    case 2:
      return rukaml_apply2(f_closure->code, f_closure->args[0], f_closure->args[1]);
      break;
    case 3:
      return rukaml_apply3(f_closure->code, f_closure->args[0], f_closure->args[1], f_closure->args[2]);
      break;
    default:
      printf("FUCK, f_closure->argsc = %lu\n", f_closure->argsc);
      printf("Application of too many arguments is not implemented!");
      assert(false);
    }
  }
  return f_closure;
}
