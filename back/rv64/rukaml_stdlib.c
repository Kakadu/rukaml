#include <alloca.h>
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

/* #define clean_errno() (errno == 0 ? "None" : strerror(errno))
#define log_error(M, ...) fprintf(stderr, "[ERROR] (%s:%d: errno: %s) " M "\n", __FILE__, __LINE__, clean_errno(), ##__VA_ARGS__)
#define assertf(A, M, ...)       \
  if (!(A))                      \
  {                              \
    log_error(M, ##__VA_ARGS__); \
    fflush(stderr);              \
    assert(A);                   \
  } */

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

#define DEBUG
#undef DEBUG

static uint64_t log_level = 0x0;

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

#define MAX_STRING_FROM_STDIN (2 << 15)

int HEAP_SIZE = 160;
const uint8_t Tuple_tag = 0;
const uint8_t Array_tag = 1;
const uint8_t Forward_tag = 250;
const uint8_t String_tag = 252;

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

// TODO(Kakadu): All rukaml functions use CC RTL on stack,
// and when we wrap function into closure, predefined functions should behave the same.
// Because of that this dirty hack. Right thing to do is to switch Rukaml calling convention to default one
void rukaml_print_int(int64_t x)
{
  printf("%s %ld\n", __func__, x);
  fflush(stdout);
}
void rukaml_print_int_kaml(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                      uint64_t a4 , uint64_t a5, uint64_t a6, uint64_t a7,
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
#define ZERO6 0,0,0,0,0,0
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
  void* rez = f(ZERO6, arg1, arg2);
#ifdef DEBUG
  printf("Returned from function with a value 0x%" PRIx64 "\n", rez);
#endif
  return rez;
}

void *rukaml_apply3(fun9 f, void *arg1, void *arg2, void *arg3)
{
#ifdef DEBUG
  printf("call %s with code ptr = 0x%" PRIx64 "\n", __func__, (uint64_t)f);
  printf("arg1 = 0x%" PRIx64 "; arg2 = 0x%" PRIx64"; arg3 = 0x%" PRIx64 "\n",
    (uint64_t)arg1, (uint64_t)arg2, (uint64_t)arg3);
  fflush(stdout);
#endif
  void* rez = f(ZERO6, arg1, arg2, arg3);
#ifdef DEBUG
  printf("Returned from function with a value 0x%" PRIx64 "\n", rez);
#endif
  return rez;
}
void *rukaml_apply4(fun10 f, void *arg1, void *arg2, void *arg3, void *arg4)
{
#ifdef DEBUG
  printf("call %s with code ptr = 0x%" PRIx64 "\n", __func__, (uint64_t)f);
  printf("arg1 = 0x%" PRIx64 "; arg2 = 0x%" PRIx64"; arg3 = 0x%" PRIx64"; arg4 = 0x%" PRIx64 "\n",
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

void *rukaml_identity(void *x) {
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
  logGC("A pair %lX created. Allocated words = %lu\n", (uint64_t)(rez + 1), GC.allocated_words);
  return rez + 1;
}

void *rukaml_alloc_block(int64_t size, uint8_t tag)
{
  if (GC.allocated_words + size + 1 > HEAP_SIZE)
  {
    fprintf(stderr, "Not enough memory\n");
    exit(1);
  }
  uint64_t **rez = ((uint64_t **)(GC.main_bank + GC.allocated_words * sizeof(void *)));
  GC.allocated_words += size + 1;
  GC.stats.gs_allocated_words += size + 1;
  rez[0] = (uint64_t *)HEADER(size, tag);
  assert(TAG(rez + 1) == tag);
  assert(SIZE(rez + 1) == size);
  logGC("A block %lX is created. Allocated words = %lu\n",
        (uint64_t)(rez + 1), GC.allocated_words);
  return rez + 1;
}

void *rukaml_alloc_array(int64_t size)
{
  return rukaml_alloc_block(size, Array_tag);
}

// Standart CC
void *rukaml_tag0(void **obj) {
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
  return SIZE(arr);
}

void **rukaml_array_stdin(void) {
  char buf[MAX_STRING_FROM_STDIN];
  int code = scanf("%s", buf);
  if (!code) {
    return rukaml_alloc_array(0);
  }
  size_t size = strlen(buf);
  void **arr = rukaml_alloc_array(size);

  for (int i = 0; i < size; i++) {
    arr[i] = (void *) (buf[i]);
  }
  return arr;
}

void **rukaml_array_read_in(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
			    uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                            void **str) {
  int len = 0;
  while (str[len++] != 0)
    ;
  char path[len];
  for (int i = 0; i < len; i++) {
    path[i] = (char)str[i];
  }

  FILE *fp = fopen(path, "r");
  if (!fp) {
    return rukaml_alloc_array(0);
  }

  fseek(fp, 0, SEEK_END);
  size_t size = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  void **arr = rukaml_alloc_array(size);

  for (int i = 0; i < size; i++) {
    int c = fgetc(fp);
    arr[i] = (void *) (c);
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
  fflush(stdout);
  // printf("\tsaved code ptr = %" PRIx64, (uint64_t)(((rukaml_closure *)f)->code));
  // fflush(stdout);

#endif
  va_list argp;
  va_start(argp, argc);
  // rukaml_closure *f_closure = copy_closure((rukaml_closure *)f);
  rukaml_closure *f_closure = f;
#ifdef DEBUG
  printf("\nf->arg_received = %lu, f->argc = %lu\n",
         f_closure->args_received,
         f_closure->argsc);
  fflush(stdout);
#endif
  // printf("f->arg_received = %u\n", f_closure->args_received);
  //  printf("%d\n", __LINE__);
  assert(f_closure->args_received + argc <= f_closure->argsc);

  if (f_closure->args_received + argc == f_closure->argsc) {
    // for full application we can omit copying closure
    void* *args_copy = malloc(f_closure->argsc * sizeof(void*));
    for (size_t i=0; i<f_closure->args_received; ++i)
      args_copy[i] = f_closure->args[i];

    // rest of the args
    for (size_t i = 0; i < argc; i++) {
      void *arg1 = va_arg(argp, void *);
      args_copy[f_closure->args_received+i] = arg1;
#ifdef DEBUG
      printf("Add new arg 0x0%x at pos %d\n", arg1, f_closure->args_received+i);
      printf("args_copy[%d] = 0x0%x\n", f_closure->args_received+i, arg1);
      fflush(stdout);
#endif
    }

    // all args in stack
#ifdef DEBUG
    for (int i=0; i<f_closure->argsc; ++i) {
      printf("args_copy[%d] = 0x0%x\n", i, args_copy[i]);
      fflush(stdout);
    }
#endif

    void** stack_args = alloca(f_closure->argsc * 8); //8 bytes per argument (int64)
    for (int i=0; i<f_closure->argsc; ++i)
      stack_args[i] = (void*)args_copy[i];

    return ((fun0)f_closure->code)();
  }

  // There we have under application
  rukaml_closure *ans_closure = copy_closure(f_closure);
  f_closure = NULL; // error avoidance


  for (size_t i = 0; i < argc; i++) {
    // printf("%d\n", __LINE__);
    void *arg = va_arg(argp, void *);
    //printf("arg[%lu] = %p, ", i, arg1);
    fflush(stdout);
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

void *rukaml_match_failure()
{
  puts("Match failure");
  fflush(stdout);
  exit(1);
}

#define PAD(n) \
  { for (unsigned int i = 0; i < n; i++) \
      printf(" "); }
void rukaml_trace_val(void *arg, unsigned int level)
{
  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wunused-parameter"
  #pragma GCC diagnostic ignored "-Wpointer-to-int-cast"

  PAD(level);
  printf("BLOCK: 0x%" PRIx64 ", tag=%lu, size=%lu" "\n", (uint64_t)arg, TAG(arg), SIZE(arg));
  // Need to implement tagged integers
  for (uint64_t i = 0; i < SIZE(arg); i++)
  {
    void **field = (void**) FIELD(arg, i);
    if ((unsigned)(*field) < 100) {
      PAD(level + 1);
      printf("%lu -> Int %ld\n", i, (int64_t)(*field));
    }
    else {
      rukaml_trace_val(*field, level  + 1);

    }
  }
  fflush(stdout);
  #pragma GCC diagnostic pop
}

uint64_t rukaml_list_length(int r0, int r1, int r2, int r3, int r4, int r5, void **ls)
{
  for (uint64_t size = 0;; ++size)
  {
    if (ls == 0) // [] lowered to int. TODO: adjust for tagged ints
    {
      return size;
    }
    else if (IS_BLOCK(ls) && (TAG(ls) == 1) && (SIZE(ls) == 2)) // ( :: ) of 'a * 'a list
    {
      ls = (void **)(ls[1]);
    }
    else
    {
      mk_err_fatal("tag mismatch (list expected)");
    }
  }
}

uint64_t rukaml_string_len_imm(void **str)
{
  if (str == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(str) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  return (uint64_t)(str[SIZE(str) - 1]);
}

void **rukaml_string_of_char_list(int r0, int r1, int r2, int r3, int r4, int r5, void **chs)
{
  // chs == 0 means [] lowered to int. TODO: adjust for tagged ints
  uint64_t chars_n = chs == 0 ? 0 : rukaml_list_length(0, 0, 0, 0, 0, 0, chs);

  uint64_t payload_words_n = (chars_n + 7) / 8;

  uint64_t *block = (uint64_t *)rukaml_alloc_block(payload_words_n + 1, String_tag);

  block[payload_words_n] = chars_n;
  assert(rukaml_string_len_imm((void **)block) == chars_n);

  for (size_t n = 0; n < chars_n; ++n)
  {
    assert(TAG(chs) == 1); // tag of ( :: )
    ((char *)(block))[n] = (char)(chs[0]);
    chs = (void **)(chs[1]);
  }

  return (void **)block;
}

void** rukaml_string_of_char_list_sysv(void** chs) {
  return rukaml_string_of_char_list(1,2,3,4,5,6,chs);
}