#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <stdbool.h>
#include <errno.h>
#include <unistd.h>

#include "rukaml_stdlib.h"

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
#undef DEBUG

static uint64_t log_level = 0;

#define logGC(...)       \
  if (log_level & 0x800) \
  printf(__VA_ARGS__)

#define HEADER(size, tag) ((uint64_t)((size << 10u) + (tag % 256u)))
#define SIZE(ptr) (*((uint64_t *)ptr - 1) >> 10)
#define TAG(ptr) (*((uint64_t *)ptr - 1) & 0xFF)
#define FIELD(ptr, n) ((uint64_t *)ptr + n)

// normal 00, gray 01, black 11
#define MAKE_WHITE(ptr) (*ptr = (*ptr & ~(0b11 << 8)))
#define MAKE_GRAY(ptr) (*ptr = (*ptr | (0b01 << 8)))
#define MAKE_BLACK(ptr) (*ptr = (*ptr | (0b11 << 8)))

int HEAP_SIZE = 160;
const uint8_t Tuple_tag = 0;
const uint8_t Array_tag = 1;
const uint8_t Forward_tag = 250;

struct gc_stats
{
  uint64_t gs_allocated_words; // allocated from beginning of the program
  uint64_t gs_current_bank;    // 0 = first bank, 1 = second
};
struct gc_data
{
  uint64_t ebp;
  uint64_t *main_bank;
  uint64_t *main_bank_fin;
  uint64_t *backup_bank;
  uint64_t *backup_bank_fin;
  uint64_t allocated_words; // currently allocated
  struct gc_stats stats;
};

static struct gc_data GC = {.ebp = 0, .allocated_words = 0, .stats = {.gs_allocated_words = 0}};

uint64_t allocated_closures = 0;


void rukaml_initialize(uint64_t ebp)
{
  setbuf(stdout, NULL);
  {
    char *env = getenv("RUKAMLRUNPARAM");
    if (env)
    {
      char *temp;
      temp = strtok(env, ",");
      while (temp != NULL)
      {
        if (strlen(temp) <= 2 || temp[1] != '=')
          continue;

        switch (temp[0])
        {
        case 'v':
        {
          uint64_t v = strtol(temp + 2, (char **)NULL, 10);
          log_level = v;
          break;
        }
        case 'm':
        {
          long v = strtol(temp + 2, (char **)NULL, 10);
          assert(v > 0);
          logGC("Setting heap size to be %ld words\n", v);
          HEAP_SIZE = (uint32_t)v;
          break;
        }
        default:
          fprintf(stderr, "Unrecongnized env switch\n");
          break;
        }
        temp = strtok(NULL, ",");
      }
    }
  }
  GC.ebp = ebp;
  logGC("%s. EBP=0x%lX\n", __func__, GC.ebp);
  const uint64_t size = sizeof(uint64_t *) * HEAP_SIZE;
  GC.main_bank = malloc(size);
  GC.main_bank_fin = GC.main_bank + size;
  GC.backup_bank = malloc(sizeof(uint64_t *) * HEAP_SIZE);
  GC.backup_bank_fin = GC.backup_bank + size;
  GC.allocated_words = 0;
  GC.stats.gs_current_bank = 0;
  logGC("main   bank: 0x%lX..0x%lX\n", (uint64_t)GC.main_bank, (uint64_t)GC.main_bank_fin);
  logGC("backup bank: 0x%lX..0x%lX\n", (uint64_t)GC.backup_bank, (uint64_t)GC.backup_bank_fin);
}

static bool is_old_bank(uint64_t *ptr)
{
  return GC.main_bank <= ptr && ptr < GC.main_bank_fin;
}

static bool is_backup_bank(uint64_t *ptr)
{
  return GC.backup_bank <= ptr && ptr < GC.backup_bank_fin;
}

void dfs(uint64_t *allocated, uint64_t *root)
{
  if (is_backup_bank(root))
    return;
  if (!is_old_bank(root))
    return;

  uint8_t tag = TAG(root);
  logGC("%s root = 0x%lX, tag = %u\n", __func__, (uint64_t)root, tag);
  if (tag == Forward_tag)
    return;
  uint64_t size = SIZE(root);
  assert(size >= 1);
  uint64_t *new_loc = (uint64_t *)GC.backup_bank + *allocated;
  logGC("new_loc = 0x%lX\n", (uint64_t)new_loc);
  *new_loc = HEADER(size, tag);
  *allocated += size + 1;

  uint64_t first_child_ptr = *root;

  logGC("Copying %lX to %lX\n", (uint64_t)root, (uint64_t)new_loc);
  root[-1] = HEADER(1, Forward_tag);
  root[0] = (uint64_t)new_loc;

  dfs(allocated, (uint64_t *)first_child_ptr);
  for (uint8_t i = 1; i < size; ++i)
    dfs(allocated, (uint64_t *)(*(root + i)));
  logGC("%s root = 0x%lX finished\n", __func__, (uint64_t)root);
}

void rukaml_gc_compact(uint64_t rsp)
{
  assert(GC.ebp > rsp);
  logGC("=== %s. EBP=0x%lX, RSP=0x%lX\n", __func__, GC.ebp, rsp);
  logGC("stack width = 0x%lX / 8\n", GC.ebp - rsp);

  uint64_t cur = GC.ebp;
  uint64_t new_size = 0;
  while (cur > rsp)
  {
    // looking for pointers, that are in the current bank
    int64_t obj = *((uint64_t *)cur);
    cur -= 8;

    if ((uint64_t)GC.main_bank <= obj && obj < (uint64_t)GC.main_bank_fin)
    {
      logGC("\t0x%lX a candidate?\n", obj);
      dfs(&new_size, (uint64_t *)obj);
    }
  }
  // cur = GC.ebp;

  //
  // while (cur > rsp)
  // {
  //   // looking for pointers, that are in the current bank
  //   int64_t obj = *((uint64_t *)cur);
  //   // printf("obj = 0x%lX, addr = 0x%lX\n", obj, cur);
  //   cur -= 8;

  //   if ((uint64_t)GC.main_bank <= obj && obj < (uint64_t)GC.main_bank_fin)
  //   {
  //     printf("\t0x%lX a candidate?\n", obj);
  //     dfs(&new_size, (uint64_t *)obj);
  //   }
  // }
  GC.allocated_words = new_size;
}

void rukaml_gc_print_stats(void)
{
  printf("GC statistics\n");
  printf("Total allocations: %ld(words)\n", GC.stats.gs_allocated_words);
  printf("Currently allocated: %ld(words)\n", GC.allocated_words);
  printf("Current bank: %ld\n", GC.stats.gs_current_bank);
  fflush(stdout);
}

void rukaml_print_alloc_closure_count (void)
{
  printf("Total closure allocations: %ld\n", allocated_closures);
  fflush(stdout);
}

void rukaml_print_int(int a0, int a1, int a2, int a3, int a4, int a5, int x)
{
  // putchar(0x30 + x);
  // putchar('\n');
  // char repr[15];
  // snprintf(repr, 15, "%d", x);
  // puts(repr);
  // printf("%s\n", __func__);
  // fflush(stdout);
  printf("%s %d\n", __func__, x);
  fflush(stdout);
}

uint64_t rukaml_array_length(int a0, int a1, int a2, int a3, int a4, int a5, void **arr)
{
  return SIZE(arr);
}

void *rukaml_array_get(int a0, int a1, int a2, int a3, int a4, int a5,
                           void **arr, uint64_t n)
{
  assert(TAG(arr) == Array_tag);
  if (n >= SIZE(arr))
  {
    fprintf(stderr, "Index out of bounds");
    exit(1);
  }
  return arr[n];
}

void rukaml_array_set(int a0, int a1, int a2, int a3, int a4, int a5,
                      void **arr, uint64_t n, void *a)
{
  assert(TAG(arr) == Array_tag);
  if (n >= SIZE(arr))
  {
    fprintf(stderr, "Index out of bounds");
    exit(1);
  }
  arr[n] = a;
  return;
}

void *rukaml_apply0(fun0 f)
{
  // TODO: I'm not sure that zero-argument call is needed
  return f();
}

// NOTE: Below we pass first 6 arguments as zeros, because they go to the registers.
// Others will go on stack
void *rukaml_apply1(fun7 foo, void *arg1)
{
#ifdef DEBUG
  printf("%s f = %" PRIx64 ", arg = %" PRIx64 "\n", __func__, foo, arg1);
#endif
  return foo(0, 0, 0, 0, 0, 0, arg1);
}

void *rukaml_apply2(fun8 f, void *arg1, void *arg2)
{
#ifdef DEBUG
  printf("call %s with code ptr = %" PRIx64 "\n", __func__, (uint64_t)f);
#endif
  return f(0, 0, 0, 0, 0, 0, arg1, arg2);
}

void *rukaml_apply3(fun9 f, void *arg1, void *arg2, void *arg3)
{
  return f(0, 0, 0, 0, 0, 0, arg1, arg2, arg3);
}
void *rukaml_apply4(fun10 f, void *arg1, void *arg2, void *arg3, void *arg4)
{
  return f(0, 0, 0, 0, 0, 0, arg1, arg2, arg3, arg4);
}
void *rukaml_apply5(fun11 f, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5)
{
  return f(0, 0, 0, 0, 0, 0, arg1, arg2, arg3, arg4, arg5);
}
void *rukaml_apply6(fun12 f, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6)
{
  return f(0, 0, 0, 0, 0, 0, arg1, arg2, arg3, arg4, arg5, arg6);
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
  if (GC.allocated_words + 3 > HEAP_SIZE)
  {
    fprintf(stderr, "Not enough memory\n");
    exit(1);
  }
  uint64_t **rez = ((uint64_t **)(GC.main_bank + GC.allocated_words * sizeof(void *)));
  GC.allocated_words += 3;
  GC.stats.gs_allocated_words += 3;
  rez[0] = (uint64_t *)HEADER(2, Tuple_tag);
  assert(TAG(rez + 1) == Tuple_tag);

  (rez)[1] = l;
  (rez)[2] = r;
  logGC("A pair %lX created. Allocated words = %lu\n", (uint64_t)(rez + 1), GC.allocated_words);
  return rez + 1;
}

void *rukaml_alloc_array(int32_t size, void** arr)
{
  if (GC.allocated_words + size + 1 > HEAP_SIZE)
  {
    fprintf(stderr, "Not enough memory\n");
    exit(1);
  }
  uint64_t **rez = ((uint64_t **)(GC.main_bank + GC.allocated_words * sizeof(void *)));
  GC.allocated_words += size + 1;
  GC.stats.gs_allocated_words += size + 1;
  rez[0] = (uint64_t *)HEADER(size, Array_tag);
  assert(TAG(rez + 1) == Array_tag);
  assert(SIZE(rez + 1) == size);
  for (unsigned int i = 1; i < size + 1; i++) {
    rez[size - i + 1] = arr[i-1];
  }
  logGC("An array %lX is created. Allocated words = %lu\n", (uint64_t)(rez + 1), GC.allocated_words);
  return rez + 1;
}

void *rukaml_field(int n, void **r)
{
  return r[n];
}

/* int64_t myadd(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t a, int64_t b)
{
  printf("a = %ld, b = %ld\n", a, b);
  return a * b;
} */

void *rukaml_alloc_closure(void *func, int32_t argsc)
{
  rukaml_closure *ans = (rukaml_closure *)malloc(sizeof(rukaml_closure) + sizeof(void *) * argsc);
  //  { .code = func, .argsc = argsc }
  ans->code = func;
  // ans->code = &myadd;
  allocated_closures += 1;
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
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
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
    case 4:
      return rukaml_apply4(f_closure->code, f_closure->args[0], f_closure->args[1], f_closure->args[2], f_closure->args[3]);
      break;
    case 5:
      return rukaml_apply5(f_closure->code, f_closure->args[0], f_closure->args[1], f_closure->args[2], f_closure->args[3], f_closure->args[4]);
      break;
    case 6:
      return rukaml_apply6(f_closure->code, f_closure->args[0], f_closure->args[1], f_closure->args[2], f_closure->args[3], f_closure->args[4], f_closure->args[5]);
      break;
    // case 7:
    //   void** stack_args = alloca(f_closure->argsc * 8); //8 bytes per argument (int64)
    //   for (int i=0; i<f_closure->argsc; ++i)
    //     stack_args[i] = (void*)f_closure->args[i];
    //   return ((fun0)f_closure->code)();
#pragma GCC diagnostic pop
    default:
      void** stack_args = alloca(f_closure->argsc * 8); //8 bytes per argument (int64)
      for (int i=0; i<f_closure->argsc; ++i)
        stack_args[i] = (void*)f_closure->args[i];
      return ((fun0)f_closure->code)();
      // printf("FUCK, f_closure->argsc = %lu\n", f_closure->argsc);
      // printf("Application of too many arguments is not implemented!");
      // assert(false);
    }
  }
  return f_closure;
}
