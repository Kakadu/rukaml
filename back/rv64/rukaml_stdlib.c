#include <assert.h>
#include <alloca.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

void __mk_err_fatal(const char *file, int line, const char *msg)
{
  fprintf(stderr, "[fatal] file=%s line=%d msg=\"%s\"\n", file, line, msg);
  fflush(stderr);
  exit(1);
}

void __mk_err_warning(const char *file, int line, const char *msg)
{
  fprintf(stderr, "[warning] file=%s line=%d msg=\"%s\"\n", file, line, msg);
}

#define mk_err_fatal(msg) __mk_err_fatal(__FILE__, __LINE__, msg)
#define mk_err_warning(msg) __mk_err_warning(__FILE__, __LINE__, msg)

// #undef RUKAML_DEBUG

#ifdef RUKAML_DEBUG
#define log(...)         \
  if (1)                 \
  {                      \
    printf(__VA_ARGS__); \
    fflush(stdout);      \
  }
#else
#define log(...)
#endif

#define HEADER(size, tag) ((uint64_t)((size << 8u) + (tag % 256u)))
#define SIZE(ptr) (*((uint64_t *)ptr - 1) >> 8)
#define TAG(ptr) (*((uint64_t *)ptr - 1) & 0xFF)
#define FIELD(ptr, n) ((uint64_t *)ptr + n)

// normal 00, gray 01, black 11
#define MAKE_WHITE(ptr) (*ptr = (*ptr & ~(0b11 << 8)))
#define MAKE_GRAY(ptr) (*ptr = (*ptr | (0b01 << 8)))
#define MAKE_BLACK(ptr) (*ptr = (*ptr | (0b11 << 8)))

#define MAX_STRING_FROM_STDIN (2 << 15)

#define IS_ON_HEAP(v) (is_backup_bank((uint64_t *)v) || is_old_bank((uint64_t *)v))
#define IS_BLOCK(v) (is_backup_bank((uint64_t *)v) || is_old_bank((uint64_t *)v))
#define IS_IMM(v) (!IS_BLOCK(v))
#define Val_unit ((value)0)
#define Val_int(n) (n)
#define Val_nil ((value)0)
#define Val_true ((value) true)
#define Val_false ((value)0)

#if defined(__riscv) && __riscv_xlen == 64
#define DECLARE_FAKE_ARGS int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7
#define FAKE_ARGS 0, 1, 2, 3, 4, 5, 6, 7
#define value uint64_t *
#define Set_field(dest, idx, newval) *((value *)dest + idx) = newval
#define Field(dest, idx) *((value *)dest + idx)

#endif

int HEAP_SIZE = 1024; // in words
const uint8_t Tuple_tag = 0;
const uint8_t Array_tag = 1;
const uint8_t Forward_tag = 250;
const uint8_t String_tag = 252;
const uint8_t Closure_tag = 247;

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
          // Loglevel temporary removed and this functionality too
          // uint64_t v = strtol(temp + 2, (char **)NULL, 10);
          // log_level = v;
          break;
        }
        case 'm':
        {
          long v = strtol(temp + 2, (char **)NULL, 10);
          assert(v > 0);
          log("Setting heap size to be %ld words\n", v);
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
  // log("%s. EBP=0x%lX\n", __func__, GC.ebp);
  const uint64_t size = sizeof(uint64_t *) * HEAP_SIZE;
  GC.main_bank = malloc(size);
  memset(GC.main_bank, 0x80, size);
  const uint64_t right_alignment = 0x20;
  uint64_t delta = (uint64_t)GC.main_bank % right_alignment;
  // log("delta = 0x%LX\n", delta);
  if (delta != 0)
  {
    GC.main_bank += (right_alignment - delta) / 8;
  }
  GC.main_bank_fin = GC.main_bank + HEAP_SIZE;
  GC.backup_bank = malloc(sizeof(uint64_t *) * HEAP_SIZE);
  GC.backup_bank_fin = GC.backup_bank + HEAP_SIZE;
  GC.allocated_words = 0;
  GC.stats.gs_current_bank = 0;
  log("main   bank: 0x%lX..0x%lX\n", (uint64_t)GC.main_bank, (uint64_t)GC.main_bank_fin);
  // log("backup bank: 0x%lX..0x%lX\n", (uint64_t)GC.backup_bank, (uint64_t)GC.backup_bank_fin);
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
  log("%s root = 0x%lX, tag = %u\n", __func__, (uint64_t)root, tag);
  if (tag == Forward_tag)
    return;
  uint64_t size = SIZE(root);
  assert(size >= 1);
  uint64_t *new_loc = (uint64_t *)GC.backup_bank + *allocated;
  log("new_loc = 0x%lX\n", (uint64_t)new_loc);
  *new_loc = HEADER(size, tag);
  *allocated += size + 1;

  uint64_t first_child_ptr = *root;

  log("Copying %lX to %lX\n", (uint64_t)root, (uint64_t)new_loc);
  root[-1] = HEADER(1, Forward_tag);
  root[0] = (uint64_t)new_loc;

  dfs(allocated, (uint64_t *)first_child_ptr);
  for (uint8_t i = 1; i < size; ++i)
    dfs(allocated, (uint64_t *)(*(root + i)));
  log("%s root = 0x%lX finished\n", __func__, (uint64_t)root);
}

void rukaml_gc_compact(uint64_t rsp)
{
  assert(GC.ebp > rsp);
  log("=== %s. EBP=0x%lX, RSP=0x%lX\n", __func__, GC.ebp, rsp);
  log("stack width = 0x%lX / 8\n", GC.ebp - rsp);

  uint64_t cur = GC.ebp;
  uint64_t new_size = 0;
  while (cur > rsp)
  {
    // looking for pointers, that are in the current bank
    int64_t obj = *((uint64_t *)cur);
    cur -= 8;

    if ((uint64_t)GC.main_bank <= obj && obj < (uint64_t)GC.main_bank_fin)
    {
      log("\t0x%lX a candidate?\n", obj);
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

// TODO(Kakadu): All rukaml functions use CC RTL on stack,
// and when we wrap function into closure, predefined functions should behave the same.
// Because of that this dirty hack. Right thing to do is to switch Rukaml calling convention to default one
void rukaml_print_int(int64_t x)
{
  printf("%s %ld\n", __func__, x);
  fflush(stdout);
}

void rukaml_print_int_kaml(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                           uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                           int64_t x)
{
  rukaml_print_int(x);
}

typedef void *(*fun0)(void);
typedef void *(*fun1)(void *);
typedef void *(*fun2)(void *, void *);
typedef void *(*fun3)(void *, void *, void *);
typedef void *(*fun7)(void *, void *, void *, void *, void *, void *, void *);
typedef void *(*fun8)(void *, void *, void *, void *, void *, void *, void *, void *);
typedef void *(*fun9)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void *(*fun10)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void *(*fun11)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

void *rukaml_apply0(fun0 f)
{
  // TODO: I'm not sure that zero-argument call is needed
  return f();
}
#define ZERO6 0, 0, 0, 0, 0, 0
// NOTE: Below we pass first 6 arguments as zeros, because they go to the registers.
// Others will go on stack
void *rukaml_apply1(fun7 foo, void *arg1)
{
#ifdef DEBUG
  printf("%s f = %" PRIx64 ", arg = %" PRIx64 "\n", __func__, foo, arg1);
#endif
  return foo(ZERO6, arg1);
}

void *rukaml_apply2(fun8 f, void *arg1, void *arg2)
{
#ifdef DEBUG
  printf("call %s with code ptr = 0x%" PRIx64 "\n", __func__, (uint64_t)f);
  printf("arg1 = 0x%" PRIx64 "; arg2 = 0x%" PRIx64 "\n", (uint64_t)arg1, (uint64_t)arg2);
  fflush(stdout);
#endif
  void *rez = f(ZERO6, arg1, arg2);
#ifdef DEBUG
  printf("Returned from function with a value 0x%" PRIx64 "\n", rez);
#endif
  return rez;
}

void *rukaml_apply3(fun9 f, void *arg1, void *arg2, void *arg3)
{
#ifdef DEBUG
  printf("call %s with code ptr = 0x%" PRIx64 "\n", __func__, (uint64_t)f);
  printf("arg1 = 0x%" PRIx64 "; arg2 = 0x%" PRIx64 "; arg3 = 0x%" PRIx64 "\n",
         (uint64_t)arg1, (uint64_t)arg2, (uint64_t)arg3);
  fflush(stdout);
#endif
  void *rez = f(ZERO6, arg1, arg2, arg3);
#ifdef DEBUG
  printf("Returned from function with a value 0x%" PRIx64 "\n", rez);
#endif
  return rez;
}
void *rukaml_apply4(fun10 f, void *arg1, void *arg2, void *arg3, void *arg4)
{
#ifdef DEBUG
  printf("call %s with code ptr = 0x%" PRIx64 "\n", __func__, (uint64_t)f);
  printf("arg1 = 0x%" PRIx64 "; arg2 = 0x%" PRIx64 "; arg3 = 0x%" PRIx64 "; arg4 = 0x%" PRIx64 "\n",
         (uint64_t)arg1, (uint64_t)arg2, (uint64_t)arg3, (uint64_t)arg4);
  fflush(stdout);
#endif
  return f(ZERO6, arg1, arg2, arg3, arg4);
}
void *rukaml_apply5(fun11 f, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5)
{
  return f(ZERO6, arg1, arg2, arg3, arg4, arg5);
}

typedef struct
{
  void *code;
  int64_t argsc; // TODO(Kakadu): uint32_t or byte ???
  int64_t args_received;
  void *args[0];
} rukaml_closure;

void *rukaml_identity(void *x)
{
  return x;
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
  log("A pair %lX created. Allocated words = %lu\n", (uint64_t)(rez + 1), GC.allocated_words);
  return rez + 1;
}

void *rukaml_alloc_block(int64_t size, uint8_t tag)
{
  if (GC.allocated_words + size + 1 > HEAP_SIZE)
  {
    fprintf(stderr, "Not enough memory\n");
    exit(1);
  }
  uint64_t *rez = (uint64_t *)(GC.main_bank + GC.allocated_words);
  GC.allocated_words += size + 1;
  GC.stats.gs_allocated_words += size + 1;
  Set_field(rez, 0, (value)HEADER(size, tag));
  uint64_t *ans = rez + 1;
  assert(TAG(ans) == tag);
  assert(SIZE(ans) == size);
  log("A block %lX is created. Allocated words = %lu\n",
      (uint64_t)(rez + 1), GC.allocated_words);
  return ans;
}

void *rukaml_alloc_closure(void *func, int32_t argsc)
{
  assert(func != NULL);
  assert(argsc > 0);
  // log("%s\n", __func__);
  // code_ptr + argsc + argmax + args[]
  size_t size = 3 + argsc;
  rukaml_closure *ans = (rukaml_closure *)rukaml_alloc_block(size, Closure_tag);
  assert(TAG(ans) == Closure_tag);
  assert(SIZE(ans) == size);
  // printf("%s ans = 0x%" PRIx64 "\n", __func__, (uint64_t*)ans); fflush(stdout);

  ans->code = func;
#ifdef DEBUG
  printf("store code ptr = %" PRIx64 "\n", (uint64_t)(ans->code));
#endif
  ans->argsc = argsc;
  ans->args_received = 0;
  memset(ans->args, 0, argsc * sizeof(void *));
#ifdef DEBUG
  printf("%s argc = %u,   %" PRIx64 "\n\n", __func__, argsc, (uint64_t)ans);
#endif
  return ans;
}

void *rukaml_alloc_array(int64_t size)
{
  return rukaml_alloc_block(size, Array_tag);
}

// Standart CC
void *rukaml_tag0(void **obj)
{
  assert(obj != NULL);
  return (void *)(uint64_t)(TAG(obj));
}

void *rukaml_tag(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                 uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                 void **obj)
{
  return rukaml_tag0(obj);
}

uint64_t rukaml_array_length(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                             uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                             void **arr)
{
  assert(arr != NULL);
  return SIZE(arr);
}

void **rukaml_array_stdin(void)
{
  char buf[MAX_STRING_FROM_STDIN];
  int code = scanf("%s", buf);
  if (!code)
  {
    return rukaml_alloc_array(0);
  }
  size_t size = strlen(buf);
  void **arr = rukaml_alloc_array(size);

  for (int i = 0; i < size; i++)
  {
    arr[i] = (void *)(buf[i]);
  }
  return arr;
}

void **rukaml_array_read_in(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                            uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                            void **str)
{
  int len = 0;
  while (str[len++] != 0)
    ;
  char path[len];
  for (int i = 0; i < len; i++)
  {
    path[i] = (char)((uint64_t)(str[i]));
  }

  FILE *fp = fopen(path, "r");
  if (!fp)
  {
    return rukaml_alloc_array(0);
  }

  fseek(fp, 0, SEEK_END);
  size_t size = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  void **arr = rukaml_alloc_array(size);

  for (int i = 0; i < size; i++)
  {
    int c = fgetc(fp);
    arr[i] = (void *)((int64_t)(c));
  }
  fread(arr, sizeof(void *), size, fp);
  fclose(fp);
  return arr;
}

void *rukaml_array_get(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                       uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
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

void rukaml_array_set(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                      uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
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

void *rukaml_field(size_t n, value r)
{
  assert(IS_ON_HEAP(r));
  assert(n < SIZE(r));
  value *arr = (value *)r;
  value ans = (arr)[n];
  if (0)
    printf("%s: field %d = 0x%" PRIx64 "\n", __func__, n, (uint64_t)ans);
  // rukaml_trace_val(ans, 3);
  return ans;
}

/* int64_t myadd(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t a, int64_t b)
{
  printf("a = %ld, b = %ld\n", a, b);
  return a * b;
} */

rukaml_closure *copy_closure(rukaml_closure *src)
{
  rukaml_closure *dst = (rukaml_closure *)rukaml_alloc_closure(src->code, src->argsc);
  dst->args_received = src->args_received;
  for (size_t i = 0; i < src->args_received; i++)
  {
    dst->args[i] = src->args[i];
  }
  assert(TAG(dst) == Closure_tag);
  assert(dst->argsc == src->argsc);
  return dst;
}

void *rukaml_applyN(void *f, int64_t argc, ...)
{
  assert(IS_ON_HEAP(f));
  assert(TAG(f) == Closure_tag);

#ifdef RUKAML_DEBUG
  printf("%s argc = %lu, closure = 0x%" PRIX64 "\n\n", __func__, argc, (uint64_t)f);
  fflush(stdout);
#endif
  va_list argp;
  va_start(argp, argc);
  rukaml_closure *f_closure = f;
#ifdef RUKAML_DEBUG
  printf("\nf->arg_received = %lu, f->argc = %lu\n",
         f_closure->args_received,
         f_closure->argsc);
  for (size_t i = 0; i < f_closure->args_received; ++i)
  {
    if (IS_ON_HEAP(f_closure->args[i]))
    {
      rukaml_trace_val(f_closure->args[i], 7);
    }
    else
      log("    Arg %d is not on heap: 0x%" PRIX64 "\n", i, f_closure->args[i]);
  }
  fflush(stdout);
#endif
  // printf("f->arg_received = %u\n", f_closure->args_received);
  //  printf("%d\n", __LINE__);
  assert(f_closure->args_received + argc <= f_closure->argsc);

  if (f_closure->args_received + argc == f_closure->argsc)
  {
    // for full application we can omit copying closure
    size_t i = 0, j;
    fun0 callable;
    // log("argsc = %d, args_received=%d, new_args=%d\n",
    //     f_closure->argsc, f_closure->args_received, argc);
    void **stack_args = alloca(f_closure->argsc * sizeof(void *));
    for (i = 0; i < f_closure->args_received; ++i)
    {
      stack_args[i] = f_closure->args[i];
      // log("Setting arg %d: 0x%lX\n", i, stack_args[i]);
      // if (IS_ON_HEAP(stack_args[i]))
      //   rukaml_trace_val(stack_args[i], 3);
      // else
      //   log(" not on heap");
    }

    // rest of the args
    for (j = f_closure->args_received; j < f_closure->argsc; j++)
    {
      // printf("Setting arg j=%d, f_closure->args_received+argc=%d\n", j, f_closure->args_received + argc);
      stack_args[j] = va_arg(argp, void *);
    }
    va_end(argp);
    callable = (fun0)f_closure->code;
    return callable();
  }
  else
  {
    // There we have under application
    rukaml_closure *ans_closure = copy_closure(f_closure);
    f_closure = NULL; // error avoidance

    for (size_t i = 0; i < argc; i++)
    {
      value arg = (value)va_arg(argp, void *);
      // printf("partial application \n", __LINE__);
      // rukaml_trace_val(arg, 3);
      // fflush(stdout);
      ans_closure->args[ans_closure->args_received++] = arg;
    }
#ifdef DEBUG
    printf("\nf->arg_received = %lu, f->argc = %lu\n",
           ans_closure->args_received,
           ans_closure->argsc);
    fflush(stdout);
#endif
    va_end(argp);

    return ans_closure;
  }
  __builtin_unreachable();
}

void *rukaml_match_failure()
{
  puts("Match failure");
  fflush(stdout);
  exit(1);
}

#define PAD(n)                           \
  {                                      \
    for (unsigned int i = 0; i < n; i++) \
      printf(" ");                       \
  }
void rukaml_trace_val(void *arg, unsigned int level)
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"

  if (!IS_ON_HEAP(arg))
  {
    printf("Out of heap value: 0x%" PRIX64 "\n", (uint64_t)arg);
    return;
  }
  PAD(level);
  printf("BLOCK:  0x%" PRIX64 ", he=0x%lX, tag=%lu, size=%lu"
         "\n",
         (uint64_t)arg, ((uint64_t *)arg)[-1], TAG(arg), SIZE(arg));
  if (TAG(arg) == String_tag)
  {
    PAD(level + 1);
    printf("\"%s\"\n", (char *)arg);
    return;
  }
  if (TAG(arg) == Closure_tag)
  {
    PAD(level + 1);
    rukaml_closure *clo = (rukaml_closure *)arg;
    printf("closure, code = 0x%" PRIx64 ", argc=%" PRId64 ", arg_got=%" PRId64 "\n",
           clo->code, clo->argsc, clo->args_received);
    return;
  }
  // Need to implement tagged integers
  for (uint64_t i = 0; i < SIZE(arg); i++)
  {
    void **field = (void **)FIELD(arg, i);
    if ((unsigned)(*field) < 100)
    {
      PAD(level + 1);
      printf("%lu -> Int %ld\n", i, (int64_t)(*field));
    }
    else
    {
      PAD(level + 1);
      printf("%lu -> ", i);
      rukaml_trace_val(*field, level + 1);
    }
  }
  fflush(stdout);
#pragma GCC diagnostic pop
}

void *rukaml_alloc_string(size_t len)
{
  size_t payload_words_n = (len + 1 + 7) / 8;
  void *block = rukaml_alloc_block(payload_words_n, String_tag);
  assert(TAG(block) == String_tag);
  assert(SIZE(block) == payload_words_n);
  memset(block, '\0', payload_words_n * sizeof(void *));
  // printf("String created at addr = 0x%lX\n", block);

  return block;
}

uint64_t rukaml_string_len_imm(value str)
{
  if (str == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  assert(IS_ON_HEAP(str));
  if (TAG(str) != String_tag)
  {
    // mk_err_fatal("tag mismatch");
    fprintf(stderr, "[warning] file=%s line=%d msg=tag mismatch, got %d\n",
            __FILE__, __LINE__, TAG(str));
    fflush(stderr);
    exit(1);
  }

  // uint64_t ans = (uint64_t)(str[SIZE(str) - 1]);
  uint64_t ans = strlen((char *)str);
  // log("Len of rukaml string '%s' is %d\n", str, ans);
  return ans;
}

void *rukaml_make_string_of_lit(const char *const s)
{
  // #ifdef RUKAML_DEBUG
  //   // printf("%s, str = '%s'\n", __func__, s); fflush(stdout);
  //   // pp_string_as_HEX( (char*)s);
  // #endif
  const size_t len = strlen(s);
  value block = (value)rukaml_alloc_string(len);

  for (size_t i = 0; i < len; ++i)
    ((char *)(block))[i] = s[i];

  // #ifdef RUKAML_DEBUG
  //   // log("block contents = ", (char*)block);
  //   // pp_string_as_HEX((char*)block);
  //   // log ("\n");
  // #endif

  assert(rukaml_string_len_imm(block) == len);
  // printf("String created at addr = 0x%lX\n", block);
  return (void *)block;
}

char rukaml_string_nth_sysv(value str, uint64_t n)
{
  assert(str != NULL);
  // log("%s n = %d, str = '%s', \n", __FUNCTION__, n, str);

  if (str == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(str) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  uint64_t str_len = 0;
  str_len = rukaml_string_len_imm(str);
  // log("%s str = '%s', n = %d, strlen = %d\n", __func__, str, n, str_len);

  if (n >= str_len)
  {
    mk_err_fatal("index out of bounds");
  }

  return ((char *)str)[n];
}

void *rukaml_output_string_sysv(int dest, value str)
{
  if (dest != 1)
  {
    log("dest = %ld\n", (int64_t)dest);
    assert(dest == 1);
  };
  // printf("str addr = 0x%lX, str = '%s', len=%d\n", str, str, strlen(str));
  if (TAG(str) != String_tag)
  {
    printf("str argument = 0x%" PRIx64 ", tag = %ld\n", (long unsigned)str, TAG(str));
    mk_err_fatal("tag mismatch");
  }
  printf("%s", (char *)str);
  // puts(str);
  fflush(stdout);
  return 0;
}

// TODO: we are currently printing only to stdout
// Need to fix this.
void rukaml_fprintf_impl(void *dest, void **fmt, va_list args)
{
  // log("%s %d\n", __func__, __LINE__);
  if (fmt == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(fmt) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  uint64_t fmt_len = rukaml_string_len_imm((value)fmt);

  // log("%s, fmtlen = %d\n", __func__, fmt_len);
  for (size_t pos = 0; pos < fmt_len; ++pos)
  {
    char ch = rukaml_string_nth_sysv((value)fmt, pos);
    // printf("ch = '%c'\n", ch);
    if (ch != '%')
    {
      // TODO: fix hardcoded stdout
      putc(ch, stdout);
      if (ch == '\n')
        fflush(stdout);
      continue;
    }
    pos++;

    if (pos >= fmt_len)
    {
      mk_err_fatal("invalid fmt");
    }

    ch = rukaml_string_nth_sysv((value)fmt, pos);

    switch (ch)
    {
    case 'a':
    {
      log("Trying to handle %%a\n");
      fflush(stdout);
      void *pp_item_closure = va_arg(args, void *);
      void *item = va_arg(args, void *);
      rukaml_applyN(pp_item_closure, 2, dest, item);
      break;
    }
    case 'b':
    {
      int64_t v = va_arg(args, int64_t);
      // TODO: fix hardcoded stdout
      fprintf(stdout, "%s", v ? "true" : "false");
      break;
    }
    case 'c':
    {
      int64_t v = va_arg(args, int64_t);
      // TODO: fix hardcoded stdout
      fprintf(stdout, "%c", (char)v);
      break;
    }
    case 'd':
    {
      int64_t v = va_arg(args, int64_t);
      // log("%s %d, dest = %d, v=%ld\n", __func__, __LINE__, dest, v);
      // TODO: fix hardcoded stdout
      fprintf(stdout, "%ld", v);
      break;
    }
    case 's':
    {
      value str = (value)va_arg(args, void **);
      // rukaml_fprintf_string(dest, str);
      // log("%s %d, dest = %d, v=%ld\n", __func__, __LINE__, dest, str);
      // log("%s, stdout = %lx, STDOUT_FILENO = %lx\n", __func__, stdout, STDOUT_FILENO);
      rukaml_output_string_sysv(1, str);
      break;
    }

    case '%':
    {
      fputc('%', dest);
      break;
    }

    default:
    {
      mk_err_fatal("invalid fmt");
      break;
    }
    }
  }

  // log("%s LEAVE\n", __func__);
}

void *rukaml_fprintf_wrap(DECLARE_FAKE_ARGS, void *out_channel, void **fmt, ...)
{
  // if (out_channel == NULL)
  // {
  //   mk_err_fatal("unexpected null ptr");
  // }
  // log("%s, ch=%X, fmt=%" PRIx64 "\n", __func__, out_channel, fmt);
  if (fmt == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(fmt) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  va_list args;
  va_start(args, fmt);
  rukaml_fprintf_impl(out_channel, fmt, args);
  va_end(args);
  return NULL;
}

// arity stands for amount of arguments which fprintf (or other format printer) expects after a format string
// for example, arity for "123" is equal to 0, for "%a" is equal to 2, for "%a %s" is equal to 3
uint64_t eval_fmt_arity(void **fmt)
{
  if (fmt == NULL)
  {
    mk_err_fatal("unexpected null");
  }
  assert(TAG(fmt) == String_tag);

  uint64_t fmt_len = rukaml_string_len_imm((value)fmt);
  // log("\n%s len = %ld\n", __func__, fmt_len);
  // pp_string_as_HEX(fmt);

  uint64_t arity_acc = 0;

  for (size_t pos = 0; pos < fmt_len; ++pos)
  {
    char ch = rukaml_string_nth_sysv((value)fmt, pos);
    // log("%d: char = '%c'\n", __LINE__, ch);
    if (ch != '%')
    {
      continue;
    }

    pos++;

    if (pos >= fmt_len)
    {
      mk_err_fatal("invalid fmt");
    }

    ch = rukaml_string_nth_sysv((value)fmt, pos);

    switch (ch)
    {
    case 'a':
      arity_acc += 2;
      break;
    case 'b':
    case 'c':
    case 'd':
    case 's':
    case 'x':
      arity_acc++;
      break;
    case '%':
      break;

    default:
      mk_err_fatal("invalid fmt");
    }
  }

  // log("%s finished\n", __func__);
  return arity_acc;
}

void *rukaml_alloc_fprintf_closure(DECLARE_FAKE_ARGS, void *out_channel, void **fmt)
{
  // log("%s %d\n", __func__, __LINE__);
  if (out_channel == NULL)
  {
    mk_err_fatal("unexpected null");
  }

  if (fmt == NULL)
  {
    mk_err_fatal("unexpected null");
  }
  assert(TAG(fmt) == String_tag);

  uint64_t arity = eval_fmt_arity(fmt);
  // log("%s, arity = %lu, fmt = '%s'\n", __func__, arity, (char *)fmt);
  // log("out_channel = %ld\n", (int64_t)out_channel);
  fflush(stdout);
  if (arity == 0)
  {
    return rukaml_fprintf_wrap(FAKE_ARGS, out_channel, fmt);
  }

  value closure = rukaml_alloc_closure((void *)rukaml_fprintf_wrap, 2 + arity);
  assert(TAG(closure) == Closure_tag);

  return rukaml_applyN(closure, 2, out_channel, fmt);
}

void *rukaml_alloc_fprintf_closure0(int dest, void **fmt)
{
  assert(TAG(fmt) == String_tag);
  return rukaml_alloc_fprintf_closure(FAKE_ARGS, dest, fmt);
}

void *rukaml_alloc_printf_closure0(void **fmt)
{
  // log("fmt = 0x%LX\n", fmt);
  assert(TAG(fmt) == String_tag);
  // log("%s %d\n", __func__, __LINE__);
  return rukaml_alloc_fprintf_closure(FAKE_ARGS, 1, fmt);
}

// SPRINTF

static void sprintf_insert_arg(value *__args_start, value *__args_fin, value x)
{
  value ans = rukaml_alloc_block(2, 0);
  if (*__args_start == NULL)
  {
    assert(*__args_fin == NULL);
    *__args_start = ans;
    Set_field(*(value **)__args_start, 0, x);
    Set_field(*(value **)__args_start, 1, Val_nil);
    // **__args_start = x;
    *__args_fin = *__args_start + 1;
    *__args_fin = *__args_start; // last/first cons cell
  }
  else
  {
    assert(*__args_fin != NULL);
    Set_field(ans, 0, x);
    Set_field(ans, 1, Val_nil);
    Set_field(*(value **)__args_fin, 1, ans);
    *__args_fin = ans;
  }
  assert(*__args_start != NULL);
  assert(*__args_fin != NULL);
  // printf("Start is: \n");
  // rukaml_trace_val(*__args_start, 0);
  // fflush(stdout);
  return;
}

static size_t eval_int_repr(uint64_t v)
{
  size_t ans = 0;
  while (true)
  {
    if (v < 10)
    {
      ans++;
      break;
    }
    if (v < 100)
    {
      ans += 2;
      break;
    }
    if (v < 1000)
    {
      ans += 3;
      break;
    }
    v = v / 1000;
    ans += 3;
  }
  return ans;
}

value rukaml_sprintf_impl(value fmt, va_list args)
{
  if (fmt == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(fmt) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  size_t pos;
  size_t fmt_len = rukaml_string_len_imm(fmt);
  size_t output_len = 1; // for '\0'
  // log("%s, fmtlen = %" PRIu64 "\n", __func__, fmt_len);
  uint64_t *__args_start = NULL;
  uint64_t *__args_fin = NULL;
  size_t argslen = 0;

  // Collection loop
  for (pos = 0; pos < fmt_len; ++pos)
  {
    char ch = rukaml_string_nth_sysv(fmt, pos);
    // printf("ch = '%c'\n", ch);
    if (ch != '%')
    {
      output_len++;
      continue;
    }

    pos++;

    if (pos >= fmt_len)
    {
      mk_err_fatal("invalid fmt");
    }

    ch = rukaml_string_nth_sysv(fmt, pos);

    switch (ch)
    {
    case 'a':
    {
      value pp_item_closure = va_arg(args, void *);
      value arg1 = va_arg(args, void *);
      value rez = rukaml_applyN(pp_item_closure, 2, Val_unit, arg1);
      argslen++;
      sprintf_insert_arg(&__args_start, &__args_fin, (void *)rez);
      break;
    }
    case 'b':
    {
      int64_t v = va_arg(args, int64_t);
      if (v == 0)
        output_len += 5;
      else
        output_len += 4;
      argslen++;
      sprintf_insert_arg(&__args_start, &__args_fin, (void *)v);
      break;
    }
    case 'c':
    {
      char c = (char)va_arg(args, int64_t);
      output_len++;
      argslen++;
      sprintf_insert_arg(&__args_start, &__args_fin, (void *)c);
      break;
    }
    case 'd':
    {
      int64_t v = va_arg(args, int64_t);
      if (v < 0)
        output_len++;
      output_len += eval_int_repr(v);
      argslen++;
      sprintf_insert_arg(&__args_start, &__args_fin, (void *)v);
      break;
    }
    case 's':
    {
      value str = (value)va_arg(args, void **);
      // rukaml_trace_val(str, 5);
      sprintf_insert_arg(&__args_start, &__args_fin, str);
      // printf("args_start = 0x%Lx\n", __args_start);
      output_len += rukaml_string_len_imm(str);
      argslen++;
      break;
    }
    case '%':
    {
      output_len++;
      break;
    }

    default:
    {
      mk_err_fatal("invalid fmt");
      break;
    }
    }
  }

  // printf("Args are collected, output_len = %lu, args_start = 0x%Lx\n", output_len, __args_start);
  // rukaml_trace_val((value)__args_start, 0);
  // fflush(stdout);
  value ans = rukaml_alloc_string(output_len);

  // Main sprintfing loop
  // log("\nMAIN sprintfing loop\n");
  // printf("rez = '%s'\n", (char*) ans);
  // fflush(stdout);

  size_t i = 0;
  for (pos = 0; pos < fmt_len; ++pos)
  {
    char ch = rukaml_string_nth_sysv(fmt, pos);

    if (ch != '%')
    {
      ((char *)ans)[i] = ch;
      i++;
      continue;
    }

    pos++;

    if (pos >= fmt_len)
    {
      mk_err_fatal("invalid fmt");
    }

    ch = rukaml_string_nth_sysv(fmt, pos);

    switch (ch)
    {
    case 'b':
    {
      assert(argslen > 0);
      bool arg = (bool)Field(__args_start, 0);
      if (arg)
      {
        memcpy((char *)ans + i, "true\0", 1 + 4);
        i += 4;
      }
      else
      {
        memcpy((char *)ans + i, "false\0", 1 + 5);
        i += 5;
      }
      __args_start = (value)Field((value *)__args_start, 1);
      break;
    }
    case 'c':
    {
      char c = (char)va_arg(args, int64_t);
      ans[i] = c;
      i++;
      __args_start = (value)Field((value *)__args_start, 1);
      break;
    }
    case 'd':
    {
      int64_t arg = (int64_t)Field(__args_start, 0);
      // log("going to print integer %ld\n", arg);
      size_t printed = sprintf((char *)ans + i, "%ld", arg);
      i += printed;
      __args_start = (value)Field((value *)__args_start, 1);
      break;
    }
    case 'a':
    case 's':
    {
      assert(argslen > 0);
      value arg = Field(__args_start, 0);
      // log("Got %%s specifier, __args_start = 0x%lx, arg = 0x%lx, arg = '%s'\n", __args_start, arg, (char*)arg);
      assert(TAG(arg) == String_tag);
      size_t l = strlen((char *)arg);
      memcpy((char *)ans + i, (char *)arg, l);
      i += l;
      argslen--;
      __args_start = (value)Field((value *)__args_start, 1);
      // printf("New args_start = %LX\n", __args_start); fflush(stdout);
      break;
    }

    case '%':
    {
      ((char *)ans)[i] = '%';
      i++;
      break;
    }

    default:
    {
      mk_err_fatal("invalid fmt");
      break;
    }
    }
  }

  // printf("Sprintf finished, output_len = %lu, rez = '%s'\n", output_len, (char*) ans);
  // pp_string_as_HEX((char*)ans);
  return ans;
}

void *rukaml_sprintf_wrap(DECLARE_FAKE_ARGS, value fmt, ...)
{
  if (fmt == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(fmt) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  va_list args;
  va_start(args, fmt);
  uint64_t *ans = rukaml_sprintf_impl(fmt, args);
  va_end(args);

  // uint64_t payload_words_n = (bytes_n + 7) / 8;                     // amount of (64-bits) words req
  // void **obj = rukaml_alloc_block(payload_words_n + 1, String_tag); // +1 stands for additional word
  // obj[payload_words_n] = (void *)bytes_n;                           // stores String.length

  // uint64_t n = fread((void *)obj, sizeof(char), bytes_n, tmp);

  return ans;
}

void *rukaml_alloc_sprintf_closure(DECLARE_FAKE_ARGS, void **fmt)
{
  if (fmt == NULL)
  {
    mk_err_fatal("unexpected null");
  }

  if (TAG(fmt) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  uint64_t arity = eval_fmt_arity(fmt);

  if (arity == 0)
  {
    return rukaml_sprintf_wrap(FAKE_ARGS, fmt);
  }

  void *closure = rukaml_alloc_closure(rukaml_sprintf_wrap, 1 + arity);

  return rukaml_applyN(closure, 1, fmt);
}

void *rukaml_alloc_sprintf_closure_sysv(void **fmt)
{
  assert(TAG(fmt) == String_tag);
  return rukaml_alloc_sprintf_closure(FAKE_ARGS, fmt);
}

// rukaml polymorphic equality for SysV ABI
value rukaml_equal_sysv(value l, value r)
{
  if (l == r)
  {
    return Val_true;
  }
  if (IS_ON_HEAP(l) && IS_ON_HEAP(r))
  {
    assert(TAG(l) == TAG(r));
    if (TAG(l) == String_tag)
    {
      int ans = strcmp((char *)l, (char *)r);
      return (ans == 0) ? Val_true : Val_false;
    }
    assert(SIZE(l) == SIZE(r));
    for (size_t i = 0; i < SIZE(l); i++)
    {
      if (Val_false == rukaml_equal_sysv(FIELD(l, i), FIELD(r, i)))
      {
        return Val_false;
      }
    }
    return Val_true;
  }

  return Val_false;
}
