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

#define MAX_STRING_FROM_STDIN (2 << 15)

int HEAP_SIZE = 1024 * 1024;
const uint8_t Tuple_tag = 0;
const uint8_t Array_tag = 1;
const uint8_t Forward_tag = 250;
const uint8_t String_tag = 252;

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

struct gc_stats
{
  uint64_t gs_allocated_words; // allocated from beginning of the program
  uint64_t gs_current_bank;    // 0 = first bank, 1 = second
};

#define INITIAL_GC_STATIC_ROOTS_CAPACITY 64

struct gc_static_roots
{
  uint64_t counter;
  uint64_t capacity;
  uint64_t **roots;
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
  struct gc_static_roots static_roots;
};

static struct gc_data GC = {
    .ebp = 0, .allocated_words = 0, .stats = {.gs_allocated_words = 0},
    // .static_roots is not compile-time constant, so it is not initialized here
};

void initialize_gc_static_roots()
{
  GC.static_roots.capacity = INITIAL_GC_STATIC_ROOTS_CAPACITY;
  GC.static_roots.roots = (uint64_t **)(calloc(INITIAL_GC_STATIC_ROOTS_CAPACITY,
                                               sizeof(uint64_t *)));
  if (GC.static_roots.roots == NULL)
  {
    mk_err_fatal("memory allocation failed");
  }
  GC.static_roots.counter = 0;
}

void teardown_gc_static_roots()
{
  free(GC.static_roots.roots);
}

// TODO! (memory leak)
//  gc roots can not only be added but also disappear, at least in two cases:
//  1. shadowing of a global variable
//  2. mutation of a global variable
//  some mechanism for removing static roots is needed
void add_gc_static_root(void **static_root)
{
  if (GC.static_roots.counter >= GC.static_roots.capacity)
  {
    uint64_t new_capacity = GC.static_roots.capacity * 2;
    uint64_t **new_roots = (uint64_t **)realloc(GC.static_roots.roots, new_capacity * sizeof(uint64_t *));
    if (new_roots == NULL)
    {
      mk_err_fatal("memory allocation failed");
    }
    GC.static_roots.capacity = new_capacity;
    GC.static_roots.roots = new_roots;
  }

  GC.static_roots.roots[GC.static_roots.counter++] = (uint64_t *)static_root;
}

uint64_t allocated_closures = 0;

static void *rukaml_sys_argv = NULL; // private field

void *rukaml_argv(void) // public getter
{
  return rukaml_sys_argv;
}

static void *rukaml_string_of_cstr(const char *cstr)
{
  size_t len = strlen(cstr);
  size_t payload_words = (len + 7) / 8;
  void **str_block = rukaml_alloc_block(payload_words + 1, String_tag);
  memcpy(str_block, cstr, len);
  ((uint64_t *)str_block)[payload_words] = len;
  return str_block;
}

void rukaml_init_argv(int argc, char **argv)
{
  if (argc < 1)
  {
    mk_err_fatal("argc < 1");
  }

#ifdef DEBUG
  printf("[debug] rukaml initialization: argc = %d\n", argc);
#endif

  void **arr = (void **)rukaml_alloc_block(argc, Array_tag);
  for (int n = 0; n < argc; n++)
  {
    char *nth = rukaml_string_of_cstr(argv[n]);
#ifdef DEBUG
    printf("[debug] rukaml initialization: argv[%d] = %s\n", n, nth);
#endif
    arr[n] = nth;
  }

  rukaml_sys_argv = arr;
}

void rukaml_initialize(uint64_t ebp, int argc, char **argv)
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
  GC.main_bank_fin = GC.main_bank + HEAP_SIZE;
  GC.backup_bank = malloc(size);
  GC.backup_bank_fin = GC.backup_bank + HEAP_SIZE;
  GC.allocated_words = 0;
  GC.stats.gs_current_bank = 0;

  initialize_gc_static_roots();
  rukaml_init_argv(argc, argv);
  add_gc_static_root(&rukaml_sys_argv); // sys_argv needs to be a static root
  ;                                     // because it's allocated via rukaml_alloc_block
  ;                                     // otherwise GC may collect its block.
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
  if (*allocated + size + 1 > HEAP_SIZE)
    mk_err_fatal("GC: out of memory in backup bank");
  uint64_t *new_loc = GC.backup_bank + *allocated + 1;
  logGC("new_loc = 0x%lX\n", (uint64_t)new_loc);
  memcpy(new_loc - 1, root - 1, (size + 1) * sizeof(uint64_t));
  *allocated += size + 1;
  logGC("Copying %lX to %lX\n", (uint64_t)root, (uint64_t)(new_loc));
  root[-1] = HEADER(1, Forward_tag);
  root[0] = (uint64_t)new_loc;
  for (uint64_t i = 0; i < size; ++i)
  {
    uint64_t *child = (uint64_t *)new_loc[i];
    if (is_old_bank(child))
    {
      if (TAG(child) != Forward_tag)
        dfs(allocated, child);
      new_loc[i] = *child;
    }
  }
  logGC("%s root = 0x%lX finished\n", __func__, (uint64_t)root);
}

void rukaml_gc_compact(uint64_t rsp)
{
  assert(GC.ebp >= rsp);
  logGC("=== %s. EBP=0x%lX, RSP=0x%lX\n", __func__, GC.ebp, rsp);
  logGC("stack width = 0x%lX / 8\n", GC.ebp - rsp);
  uint64_t cur = GC.ebp;
  uint64_t new_size = 0;

  logGC("begin stack walking\n");
  for (uint64_t cur = GC.ebp; cur >= rsp; cur -= 8)
  {
    uint64_t *obj = *((uint64_t **)cur);
    if (GC.main_bank <= obj && obj < GC.main_bank_fin)
    {
      if (SIZE(obj) > HEAP_SIZE)
        continue;

      logGC("\t0x%lX a candidate?\n", (uint64_t)obj);
      if (TAG(obj) != Forward_tag)
      {
        dfs(&new_size, obj);
      }

      *((uint64_t *)cur) = *obj;
    }
  }

  logGC("begin static roots walking\n");
  for (uint64_t i = 0; i < GC.static_roots.counter; i++)
  {
    uint64_t *root = GC.static_roots.roots[i];
    uint64_t *obj = (uint64_t *)(*root);
    if (!is_old_bank(obj) || is_backup_bank(obj))
      continue;

    if (TAG(obj) != Forward_tag)
      dfs(&new_size, obj);

    *root = *obj;
  }

  GC.allocated_words = new_size;

  // swap banks
  uint64_t *tmp = GC.main_bank;
  GC.main_bank = GC.backup_bank;
  GC.backup_bank = tmp;
  GC.main_bank_fin = GC.main_bank + HEAP_SIZE;
  GC.backup_bank_fin = GC.backup_bank + HEAP_SIZE;
}

void rukaml_gc_print_stats(void)
{
  printf("GC statistics\n");
  printf("Total allocations: %ld(words)\n", GC.stats.gs_allocated_words);
  printf("Currently allocated: %ld(words)\n", GC.allocated_words);
  printf("Current bank: %ld\n", GC.stats.gs_current_bank);
  fflush(stdout);
}

void rukaml_print_alloc_closure_count(void)
{
  printf("Total closure allocations: %ld\n", allocated_closures);
  fflush(stdout);
}

// TODO: implement tagged int's to distinguish immediate values from heap blocks
// properly

// good implementation:
// #define IS_IMM(v) (((uint64_t)(v) & 1) == 1)
// #define IS_BLOCK(v) (((uint64_t)(v) & 1) == 0)
// #define BOX_IMM(v) (((int64_t)(v) << 1) | 1)
// #define UNBOX_IMM(v) (((int64_t)(v) >> 1))

// bad implementation:
#define IS_BLOCK(v) (is_backup_bank((uint64_t *)v) || is_old_bank((uint64_t *)v))
#define IS_IMM(v) (!IS_BLOCK(v))

void rukaml_print_int(int64_t x)
{
  printf("%s %d\n", __func__, x);
  fflush(stdout);
}

void rukaml_print_int_kaml(int a0, int a1, int a2, int a3, int a4, int a5, int64_t x)
{
  rukaml_print_int(x);
}

void **rukaml_array_stdin(void)
{
  char buf[MAX_STRING_FROM_STDIN];
  int code = scanf("%s", buf);
  if (!code)
  {
    return rukaml_alloc_block(0, Array_tag);
  }
  size_t size = strlen(buf);
  void **arr = rukaml_alloc_block(size, Array_tag);

  for (int i = 0; i < size; i++)
  {
    arr[i] = (void *)(buf[i]);
  }
  return arr;
}

void **rukaml_array_read_in(int a0, int a1, int a2, int a3, int a4, int a5,
                            void **str)
{
  int len = 0;
  while (str[len++] != 0)
    ;
  char path[len];
  for (int i = 0; i < len; i++)
  {
    path[i] = (char)str[i];
  }

  FILE *fp = fopen(path, "r");
  if (!fp)
  {
    return rukaml_alloc_block(0, Array_tag);
  }

  fseek(fp, 0, SEEK_END);
  size_t size = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  void **arr = rukaml_alloc_block(size, Array_tag);

  for (int i = 0; i < size; i++)
  {
    int c = fgetc(fp);
    arr[i] = (void *)(c);
  }
  fread(arr, sizeof(void *), size, fp);
  fclose(fp);
  return arr;
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

void rukaml_match_failure()
{
  fprintf(stderr, "Match failure\n");
  exit(1);
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

void *rukaml_alloc_block(uint64_t size, uint64_t tag)
{
  if (GC.allocated_words + size + 1 > HEAP_SIZE)
  {
    mk_err_fatal("Not enough memory");
  }
  uint64_t **rez = (uint64_t **)(GC.main_bank + GC.allocated_words);
  GC.allocated_words += size + 1;
  GC.stats.gs_allocated_words += size + 1;
  *rez = (uint64_t *)HEADER(size, tag);
  assert(SIZE(rez + 1) == size);
  assert(TAG(rez + 1) == tag);

  logGC("A block %lX is created. Allocated words = %lu\n", (uint64_t)(rez + 1), GC.allocated_words);

  return rez + 1;
}

uint64_t rukaml_block_tag(int, int, int, int, int, int, void **obj)
{
  // constant adt variants are lowered to int (TODO: adjust for tagged ones)
  if (IS_IMM(obj))
  {
    return (uint64_t)(obj);
  }

  return TAG(obj);
}

uint64_t rukaml_block_size(int, int, int, int, int, int, void **obj)
{
  return SIZE(obj);
}

void *rukaml_block_nth(int, int, int, int, int, int, void **obj, uint64_t n)
{
  return obj[n];
}

void *rukaml_field(void **obj, uint64_t n)
{
  return obj[n];
}

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
      void **stack_args = alloca(f_closure->argsc * 8); // 8 bytes per argument (int64)
      for (int i = 0; i < f_closure->argsc; ++i)
        stack_args[i] = (void *)f_closure->args[i];
      return ((fun0)f_closure->code)();
      // printf("FUCK, f_closure->argsc = %lu\n", f_closure->argsc);
      // printf("Application of too many arguments is not implemented!");
      // assert(false);
    }
  }
  return f_closure;
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

char rukaml_string_nth_imm(void **str, uint64_t n)
{
  if (str == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(str) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  uint64_t str_len = rukaml_string_len_imm(str);

  if (n >= str_len)
  {
    mk_err_fatal("index out of bounds");
  }

  return ((char *)str)[n];
}

char rukaml_string_nth(int r0, int r1, int r2, int r3, int r4, int r5, void **str, uint64_t n)
{
  return rukaml_string_nth_imm(str, n);
}

char rukaml_string_len(int r0, int r1, int r2, int r3, int r4, int r5, void **str)
{
  return rukaml_string_len_imm(str);
}

uint64_t rukaml_list_length(int r0, int r1, int r2, int r3, int r4, int r5,
                            void **ls)
{
  for (uint64_t size = 0;; ++size)
  {
    if (ls == 0) // [] lowered to int. TODO: adjust for tagged ints
    {
      return size;
    }
    else if (IS_BLOCK(ls) && (TAG(ls) == 1) &&
             (SIZE(ls) == 2)) // ( :: ) of 'a * 'a list
    {
      ls = (void **)(ls[1]);
    }
    else
    {
      mk_err_fatal("tag mismatch");
    }
  }
}

void **rukaml_string_of_char_list(int r0, int r1, int r2, int r3, int r4,
                                  int r5, void **chs)
{
  uint64_t chars_n = rukaml_list_length(0, 0, 0, 0, 0, 0, chs);
  uint64_t payload_words_n = (chars_n + 7) / 8;
  uint64_t *block =
      (uint64_t *)rukaml_alloc_block(payload_words_n + 1, String_tag);

  block[payload_words_n] = chars_n;
  assert(rukaml_string_len_imm((void **)block) == chars_n);

  for (size_t n = 0; n < chars_n; ++n)
  {
    assert(TAG(chs) == 1);                 // tag of ( :: )
    ((char *)(block))[n] = (char)(chs[0]); // get head
    chs = (void **)(chs[1]);               // get next list node
  }

  return (void **)block;
}

bool rukaml_string_equal(int r0, int r1, int r2, int r3, int r4, int r5, void **left, void **right)
{
  if (left == NULL)
  {
    mk_err_fatal("unexpected null");
  }

  if (right == NULL)
  {
    mk_err_fatal("unexpected null");
  }

  if (TAG(left) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  if (TAG(right) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  if (rukaml_string_len_imm(left) != rukaml_string_len_imm(right))
  {
    return false;
  }

  return memcmp(left, right, rukaml_string_len_imm(left)) == 0;
}
void *rukaml_stdin(void)
{
  return (void *)stdin;
}

void *rukaml_stderr(void)
{
  return (void *)stderr;
}

void *rukaml_stdout(void)
{
  return (void *)stdout;
}

void *rukaml_open_impl(void **path, const char *mode)
{
  if (path == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(path) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  if (mode == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  uint64_t len = rukaml_string_len_imm(path);

  char *path_cstr = malloc(len + 1);

  if (path_cstr == NULL)
  {
    mk_err_fatal("memory allocation failed");
  }

  memcpy(path_cstr, (const void *)path, len);

  path_cstr[len] = '\0';

  FILE *file = fopen(path_cstr, mode);

  if (file == NULL)
  {
    mk_err_fatal("fopen failed");
  }

  free(path_cstr);

  return (void *)file;
}

void *rukaml_open_in(int r0, int r1, int r2, int r3, int r4, int r5, void **path)
{
  return rukaml_open_impl(path, "r");
}

void *rukaml_open_out(int r0, int r1, int r2, int r3, int r4, int r5, void **path)
{
  return rukaml_open_impl(path, "w");
}

void rukaml_close_channel(int r0, int r1, int r2, int r3, int r4, int r5, void *channel)
{
  if (channel == NULL)
  {
    mk_err_warning("unexpected null");
  }
  else
  {
    fclose((FILE *)channel);
  }
}

int64_t rukaml_input_char(int r0, int r1, int r2, int r3, int r4, int r5, void *channel)
{
  return fgetc((FILE *)channel);
}

int64_t rukaml_end_of_input(int r0, int r1, int r2, int r3, int r4, int r5, void *channel)
{
  int ch = fgetc((FILE *)channel);

  if (ch == EOF)
  {
    return true;
  }

  ungetc(ch, (FILE *)channel);

  return false;
}

void rukaml_fwrite_string(FILE *dest, void **str)
{
  if (dest == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (str == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(str) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  uint64_t str_len = rukaml_string_len_imm(str);

  uint64_t n = fwrite((void *)(str), sizeof(char), str_len, dest);

  if (n != str_len)
  {
    mk_err_warning("fwrite failed");
  }
}

void rukaml_fprintf_impl(FILE *dest, void **fmt, va_list args)
{
  if (dest == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (fmt == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(fmt) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  uint64_t fmt_len = rukaml_string_len_imm(fmt);

  for (size_t pos = 0; pos < fmt_len; ++pos)
  {
    char ch = rukaml_string_nth_imm(fmt, pos);

    if (ch != '%')
    {
      fputc(ch, dest);
      continue;
    }

    pos++;

    if (pos >= fmt_len)
    {
      mk_err_fatal("invalid fmt");
    }

    ch = rukaml_string_nth_imm(fmt, pos);

    switch (ch)
    {
    case 'a':
    {
      void *pp_item_closure = va_arg(args, void *);
      void *item = va_arg(args, void *);
      rukaml_applyN(pp_item_closure, 2, dest, item);
      break;
    }
    case 'b':
    {
      int64_t v = va_arg(args, int64_t);
      fprintf(dest, "%s", v ? "true" : "false");
      break;
    }
    case 'c':
    {
      int64_t v = va_arg(args, int64_t);
      fprintf(dest, "%c", (char)v);
      break;
    }
    case 'd':
    {
      int64_t v = va_arg(args, int64_t);
      fprintf(dest, "%ld", v);
      break;
    }
    case 's':
    {
      void **str = va_arg(args, void **);
      rukaml_fwrite_string(dest, str);
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
}

void *rukaml_fprintf_wrap(int a0, int a1, int a2, int a3, int a4, int a5, void *out_channel, void **fmt, ...)
{
  if (out_channel == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

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
  rukaml_fprintf_impl((FILE *)out_channel, fmt, args);
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

  if (TAG(fmt) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  uint64_t fmt_len = rukaml_string_len_imm(fmt);

  uint64_t arity_acc = 0;

  for (size_t pos = 0; pos < fmt_len; ++pos)
  {
    char ch = rukaml_string_nth_imm(fmt, pos);

    if (ch != '%')
    {
      continue;
    }

    pos++;

    if (pos >= fmt_len)
    {
      mk_err_fatal("invalid fmt");
    }

    ch = rukaml_string_nth_imm(fmt, pos);

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

  return arity_acc;
}

void *rukaml_alloc_fprintf_closure(int a0, int a1, int a2, int a3, int a4, int a5, void *out_channel, void **fmt)
{
  if (out_channel == NULL)
  {
    mk_err_fatal("unexpected null");
  }

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
    return rukaml_fprintf_wrap(0, 0, 0, 0, 0, 0, out_channel, fmt);
  }

  void *closure = rukaml_alloc_closure(rukaml_fprintf_wrap, 2 + arity);

  return rukaml_applyN(closure, 2, out_channel, fmt);
}

void *rukaml_alloc_printf_closure(int a0, int a1, int a2, int a3, int a4, int a5, void **fmt)
{
  return rukaml_alloc_fprintf_closure(0, 0, 0, 0, 0, 0, stdout, fmt);
}

void *rukaml_sprintf_wrap(int a0, int a1, int a2, int a3, int a4, int a5, void **fmt, ...)
{
  if (fmt == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(fmt) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  FILE *tmp = tmpfile();

  if (tmp == NULL)
  {
    mk_err_fatal("can not create temp file");
  }

  va_list args;
  va_start(args, fmt);
  rukaml_fprintf_impl(tmp, fmt, args);
  va_end(args);

  uint64_t bytes_n = ftell(tmp); // amount of bytes which fprintf_impl have stored to temp file
  rewind(tmp);

  uint64_t payload_words_n = (bytes_n + 7) / 8;                     // amount of (64-bits) words required to store bytes of string and pading
  void **obj = rukaml_alloc_block(payload_words_n + 1, String_tag); // +1 stands for additional word at the end to store String.length
  obj[payload_words_n] = (void *)bytes_n;                           // stores String.length

  uint64_t n = fread((void *)obj, sizeof(char), bytes_n, tmp);

  if (n != bytes_n)
  {
    mk_err_fatal("fread failed");
  }

  return obj;
}

void *rukaml_alloc_sprintf_closure(int a0, int a1, int a2, int a3, int a4,
                                   int a5, void **fmt)
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
    return rukaml_sprintf_wrap(0, 0, 0, 0, 0, 0, fmt);
  }

  void *closure = rukaml_alloc_closure(rukaml_sprintf_wrap, 1 + arity);

  return rukaml_applyN(closure, 1, fmt);
}

uint64_t rukaml_equal_struct(void **left, void **right)
{
  if (IS_IMM(left) && IS_IMM(right))
  {
    return left == right;
  }

  if (IS_IMM(left) || IS_IMM(right))
  {
    return false;
  }

  if ((TAG(left) != TAG(right)) || (SIZE(left) != SIZE(right)))
  {
    return false;
  }

  for (size_t n = 0; n < SIZE(left); n++)
  {
    if (!rukaml_equal_struct((void **)(left[n]), (void **)(right[n])))
    {
      return false;
    }
  }

  return true;
}
