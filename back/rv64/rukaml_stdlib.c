#include <alloca.h>
#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void __mk_err_fatal(const char *file, int line, const char *msg) {
  fprintf(stderr, "[fatal] file=%s line=%d msg=\"%s\"\n", file, line, msg);
  fflush(stderr);
  exit(1);
}

void __mk_err_warning(const char *file, int line, const char *msg) {
  fprintf(stderr, "[warning] file=%s line=%d msg=\"%s\"\n", file, line, msg);
}

#define mk_err_fatal(msg) __mk_err_fatal(__FILE__, __LINE__, msg)
#define mk_err_warning(msg) __mk_err_warning(__FILE__, __LINE__, msg)

// #undef RUKAML_DEBUG

// clang-format off
#if RUKAML_DEBUG == 1
#define log(...) \
  if (1) { \
    printf(__VA_ARGS__); \
    fflush(stdout);       \
  }
#else
#define log(...)
#endif
// clang-format on

// normal 00, gray 01, black 11
#define MAKE_WHITE(ptr) (*ptr = (*ptr & ~(0b11 << 8)))
#define MAKE_GRAY(ptr) (*ptr = (*ptr | (0b01 << 8)))
#define MAKE_BLACK(ptr) (*ptr = (*ptr | (0b11 << 8)))

#define MAX_STRING_FROM_STDIN (2 << 15)

#define IS_ON_HEAP(v) (is_backup_bank(v) || is_old_bank(v))
#define IS_BLOCK(v) (is_backup_bank(v) || is_old_bank(v))
#define IS_IMM(v) (!IS_BLOCK(v))
#define Val_unit ((value)0)
#define Val_int(n) ((value)n)
#define Val_nil ((value)0)
#define Val_true ((value) true)
#define Val_false ((value)0)

#if defined(__riscv)
#define DECLARE_FAKE_ARGS                                                      \
  int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7
#define FAKE_ARGS 0, 1, 2, 3, 4, 5, 6, 7

#if __riscv_xlen == 64
#define HEADER(size, tag) ((uint64_t)((size << 8u) + (tag % 256u)))
#define SIZE(ptr) (*((uint64_t *)ptr - 1) >> 8)
#define TAG(ptr) (*((uint64_t *)ptr - 1) & 0xFF)

typedef int64_t *value;
typedef int64_t rukaml_int_t;
typedef uint64_t rukaml_uint_t;
#define Int_val(n) ((long)n)
const size_t rukaml_word_size = 8;
#define PRIxVAL "lx"
#define PRIdVAL "ld"
#endif

#if __riscv_xlen == 32
#define HEADER(size, tag) ((uint32_t)((size << 8u) + (tag % 256u)))
#define SIZE(ptr) (*((uint32_t *)ptr - 1) >> 8)
#define TAG(ptr) ((uint8_t)(*((uint32_t *)ptr - 1) & 0xFF))

typedef int32_t *value;
typedef int32_t rukaml_int_t;
typedef uint32_t rukaml_uint_t;
#define Int_val(n) ((int32_t)n)
const size_t rukaml_word_size = 4;
#define PRIxVAL "x"
#define PRIdVAL "d"
#endif

#define Set_field(dest, idx, newval) *((value *)dest + idx) = newval
#define Field(dest, idx) *((value *)dest + idx)
#endif

#define Clo_code(ans) (Field(ans, 0))
#define Clo_arity(ans) (Field(ans, 1))
#define Clo_received(ans) (Field(ans, 2))
#define Clo_arg(ans, i) (Field(ans, 3 + i))

#define Set_clo_code(ans, code) (ans[0] = code)
#define Set_clo_arity(ans, v) Set_field(ans, 1, v)
#define Set_clo_received(ans, v) Set_field(ans, 2, v)
#define Set_clo_arg(ans, i, v) Set_field(ans, 3 + i, v)

int HEAP_SIZE = 8 * 4 * 4 * 1024; // in words
const uint8_t Tuple_tag = 0;
const uint8_t Array_tag = 1;
const uint8_t Forward_tag = 250;
const uint8_t String_tag = 252;
const uint8_t Closure_tag = 247;

struct gc_stats {
  size_t gs_allocated_words; // allocated from beginning of the program
  size_t gs_current_bank;    // 0 = first bank, 1 = second
};
struct gc_data {
  uint64_t ebp;
  uintptr_t main_bank;
  uintptr_t main_bank_fin;
  uintptr_t backup_bank;
  uintptr_t backup_bank_fin;
  rukaml_uint_t allocated_words; // currently allocated
  struct gc_stats stats;
};

value rukaml_alloc_array(value size);
value rukaml_alloc_string(value len);
void rukaml_trace_val(value arg, unsigned int level);

static struct gc_data GC = {
    .ebp = 0, .allocated_words = 0, .stats = {.gs_allocated_words = 0}};

value _argv = NULL;
value rukaml_get_argv(void) {
  assert(_argv);
  assert(TAG(_argv) == Array_tag);
  return _argv;
}

void rukaml_initialize(size_t ebp, size_t argc, char **argv) {
  setbuf(stdout, NULL);
  {
    char *env = getenv("RUKAMLRUNPARAM");
    if (env) {
      char *temp;
      temp = strtok(env, ",");
      while (temp != NULL) {
        if (strlen(temp) <= 2 || temp[1] != '=')
          continue;

        switch (temp[0]) {
        case 'v': {
          // Loglevel temporary removed and this functionality too
          // uint64_t v = strtol(temp + 2, (char **)NULL, 10);
          // log_level = v;
          break;
        }
        case 'm': {
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
  const uint32_t size = rukaml_word_size * HEAP_SIZE;
  GC.main_bank = (uintptr_t)malloc(size);
  memset((void *)GC.main_bank, 0x80, size);
  log("initial bank starts at 0x%" PRIxVAL "\n", GC.main_bank);
  const rukaml_uint_t right_alignment = 0x10;
  rukaml_uint_t delta = (rukaml_uint_t)(GC.main_bank) % right_alignment;
  // log("delta = 0x%" PRIxPTR "\n", delta);
  assert(delta < right_alignment);
  if (delta != 0) {
    GC.main_bank += (right_alignment - delta);
  }
  GC.main_bank_fin = GC.main_bank + HEAP_SIZE;
  GC.backup_bank = (uintptr_t)malloc(size);
  GC.backup_bank_fin = GC.backup_bank + HEAP_SIZE;
  GC.allocated_words = 0;
  GC.stats.gs_current_bank = 0;
  log("main   bank: 0x%" PRIxVAL "..0x%" PRIxVAL "\n", GC.main_bank,
      GC.main_bank_fin);
  // log("backup bank: 0x%lX..0x%lX\n", (uint64_t)GC.backup_bank,
  // (uint64_t)GC.backup_bank_fin);

  // Initialize argv
  _argv = rukaml_alloc_array(Val_int(argc));
  assert(SIZE(_argv) == argc);
  for (size_t i = 0; i < argc; i++) {
    char *s = argv[i];
    int len = strlen(s);
    value _str = rukaml_alloc_string(Val_int(len));
    assert(TAG(_str) == String_tag);
    strcpy((char *)_str, s);
    Set_field(_argv, i, _str);
  }
}

static bool is_old_bank(value ptr) {
  return GC.main_bank <= ptr && ptr < GC.main_bank_fin;
}

static bool is_backup_bank(value ptr) {
  return GC.backup_bank <= ptr && ptr < GC.backup_bank_fin;
}

void dfs(size_t *allocated, value root) {
  if (is_backup_bank(root))
    return;
  if (!is_old_bank(root))
    return;

  uint8_t tag = TAG(root);
  log("%s root = 0x%lX, tag = %u\n", __func__, (uint64_t)root, tag);
  if (tag == Forward_tag)
    return;
  size_t size = SIZE(root);
  assert(size >= 1);
  value new_loc = (value)GC.backup_bank + *allocated * rukaml_word_size;
  log("new_loc = 0x%lX\n", (uint64_t)new_loc);
  *new_loc = HEADER(size, tag);
  *allocated += size + 1;

  uint64_t first_child_ptr = *root;

  log("Copying %lX to %lX\n", (uint64_t)root, (uint64_t)new_loc);
  Set_field(root, -1, (value)HEADER(1, Forward_tag));
  Set_field(root, 0, new_loc);

  for (size_t i = 0; i < size; ++i)
    dfs(allocated, Field(root, i));
  log("%s root = 0x%" PRIxPTR " finished\n", __func__, root);
}

void rukaml_gc_compact_sysv(size_t rsp) {
  assert(GC.ebp > rsp);
  log("=== %s. EBP=0x%lX, RSP=0x%lX\n", __func__, GC.ebp, rsp);
  log("stack width = 0x%lX / 8\n", GC.ebp - rsp);

  value *cur = (value *)GC.ebp;
  size_t new_size = 0;
  while (cur > rsp) {
    // looking for pointers, that are in the current bank
    value obj = *cur;
    cur -= 1;

    if (is_old_bank(obj)) {
      log("\t0x%lX a candidate?\n", obj);
      dfs(&new_size, obj);
    }
  }
  GC.allocated_words = new_size;
}

void rukaml_gc_stats_sysv(void) {
  printf("GC statistics\n");
  printf("Total allocations: %ld(words)\n", GC.stats.gs_allocated_words);
  printf("Currently allocated: %ld(words)\n", GC.allocated_words);
  printf("Current bank: %ld\n", GC.stats.gs_current_bank);
  fflush(stdout);
}

// clang-format off
#define PAD(n) \
  { for (size_t i = 0; i < n; i++) \
      putc(' ',stdout); \
  }
// clang-format on

void rukaml_trace_val(value arg, unsigned int level) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"

  if (!IS_ON_HEAP(arg)) {
    printf("Out of heap value: 0x%" PRIxVAL "\n", arg);
    return;
  }
  PAD(level);
  printf("BLOCK:  0x%" PRIxVAL ", he=0x%" PRIxVAL ", tag = %hhu, size=%u"
         "\n",
         arg, Field(arg, -1), TAG(arg), SIZE(arg));
  fflush(stdout);
  if (TAG(arg) == String_tag) {
    PAD(level + 1);
    printf("\"%s\"\n", (char *)arg);
    fflush(stdout);
    return;
  }
  if (TAG(arg) == Closure_tag) {
    PAD(level + 1);
    printf("closure, code = 0x%" PRIxVAL ", argc = %" PRIdVAL
           ", arg_got = %" PRIdVAL "\n",
           Clo_code(arg), Clo_arity(arg), Clo_received(arg));
    for (size_t i = 0; i < Clo_received((arg)); i++) {
      PAD(level + 5);
      printf("%u: ", i);
      rukaml_trace_val(Clo_arg(arg, i), level + 6);
      printf("\n");
    }
    return;
  }
  // Need to implement tagged integers
  for (uint64_t i = 0; i < SIZE(arg); i++) {
    printf("i = %d\n", i);
    fflush(stdout);
    value field = Field(arg, i);
    if ((unsigned)(field) < 100) {
      PAD(level + 1);
      printf("%lu -> Int %ld\n", i, (int64_t)(field));
    } else {
      PAD(level + 1);
      printf("%lu -> ", i);
      rukaml_trace_val(field, level + 1);
    }
  }
  fflush(stdout);
#pragma GCC diagnostic pop
}

// TODO(Kakadu): All rukaml functions use CC RTL on stack,
// and when we wrap function into closure, predefined functions should behave
// the same. Because of that this dirty hack. Right thing to do is to switch
// Rukaml calling convention to default one
void rukaml_print_int(value x) {
  // printf("%s %" PRIdVAL " 0x%" PRIxVAL "\n", __func__, x, x);
  printf("%s %" PRIdVAL "\n", __func__, x);
  fflush(stdout);
}

void rukaml_print_int_kaml(DECLARE_FAKE_ARGS, value x) { rukaml_print_int(x); }

typedef void *(*fun0)(void);
typedef void *(*fun1)(void *);
typedef void *(*fun2)(void *, void *);
typedef void *(*fun3)(void *, void *, void *);
typedef void *(*fun7)(void *, void *, void *, void *, void *, void *, void *);
typedef void *(*fun8)(void *, void *, void *, void *, void *, void *, void *,
                      void *);
typedef void *(*fun9)(void *, void *, void *, void *, void *, void *, void *,
                      void *, void *);
typedef void *(*fun10)(void *, void *, void *, void *, void *, void *, void *,
                       void *, void *, void *);
typedef void *(*fun11)(void *, void *, void *, void *, void *, void *, void *,
                       void *, void *, void *, void *);

void *rukaml_apply0(fun0 f) {
  // TODO: I'm not sure that zero-argument call is needed
  return f();
}
// NOTE: Below we pass first 6 arguments as zeros, because they go to the
// registers. Others will go on stack
// void *rukaml_apply1(fun9 foo, void *arg1) {
// #ifdef DEBUG
//   printf("%s f = %" PRIx64 ", arg = %" PRIx64 "\n", __func__, foo, arg1);
// #endif
//   return foo(FAKE_ARGS, arg1);
// }

// void *rukaml_apply2(fun8 f, void *arg1, void *arg2) {
// #ifdef DEBUG
//   printf("call %s with code ptr = 0x%" PRIx64 "\n", __func__, (uint64_t)f);
//   printf("arg1 = 0x%" PRIx64 "; arg2 = 0x%" PRIx64 "\n", (uint64_t)arg1,
//          (uint64_t)arg2);
//   fflush(stdout);
// #endif
//   void *rez = f(FAKE_ARGS, arg1, arg2);
// #ifdef DEBUG
//   printf("Returned from function with a value 0x%" PRIx64 "\n", rez);
// #endif
//   return rez;
// }

// void *rukaml_apply3(fun9 f, void *arg1, void *arg2, void *arg3) {
// #ifdef DEBUG
//   printf("call %s with code ptr = 0x%" PRIx64 "\n", __func__, (uint64_t)f);
//   printf("arg1 = 0x%" PRIx64 "; arg2 = 0x%" PRIx64 "; arg3 = 0x%" PRIx64
//   "\n",
//          (uint64_t)arg1, (uint64_t)arg2, (uint64_t)arg3);
//   fflush(stdout);
// #endif
//   void *rez = f(FAKE_ARGS, arg1, arg2, arg3);
// #ifdef DEBUG
//   printf("Returned from function with a value 0x%" PRIx64 "\n", rez);
// #endif
//   return rez;
// }
// void *rukaml_apply4(fun10 f, void *arg1, void *arg2, void *arg3, void *arg4)
// { #ifdef DEBUG
//   printf("call %s with code ptr = 0x%" PRIx64 "\n", __func__, (uint64_t)f);
//   printf("arg1 = 0x%" PRIx64 "; arg2 = 0x%" PRIx64 "; arg3 = 0x%" PRIx64
//          "; arg4 = 0x%" PRIx64 "\n",
//          (uint64_t)arg1, (uint64_t)arg2, (uint64_t)arg3, (uint64_t)arg4);
//   fflush(stdout);
// #endif
//   return f(FAKE_ARGS, arg1, arg2, arg3, arg4);
// }
// void *rukaml_apply5(fun11 f, void *arg1, void *arg2, void *arg3, void *arg4,
//                     void *arg5) {
//   return f(FAKE_ARGS, arg1, arg2, arg3, arg4, arg5);
// }

// void *rukaml_identity(void *x) { return x; }

value rukaml_alloc_pair(value l, value r) {
  if (GC.allocated_words + 3 > HEAP_SIZE) {
    fprintf(stderr, "Not enough memory\n");
    exit(1);
  }
  value rez = (value)((rukaml_int_t)GC.main_bank +
                      GC.allocated_words * rukaml_word_size);
  GC.allocated_words += 3;
  GC.stats.gs_allocated_words += 3;
  Set_field(rez, 0, (value)HEADER(2, Tuple_tag));
  assert(TAG(rez + rukaml_word_size) == Tuple_tag);

  Set_field(rez, 1, l);
  Set_field(rez, 2, r);

  log("A pair %" PRIxVAL " created. Allocated words = %lu\n",
      (value)(rez + rukaml_word_size), GC.allocated_words);
  return rez + 1;
}

value rukaml_alloc_block(size_t size, uint8_t tag) {
  // printf("\n%s, size = %lu, tag = %u\n", __func__, size,
  //        ((unsigned)tag % 0xFF));
  if (GC.allocated_words + size + 1 > HEAP_SIZE) {
    fprintf(stderr, "Not enough memory\n");
    exit(1);
  }
  value rez = (value)((rukaml_uint_t)GC.main_bank +
                      rukaml_word_size * GC.allocated_words);
  // printf("rez = 0x%" PRIxVAL "\n", rez);
  GC.allocated_words += (size + 1);
  GC.stats.gs_allocated_words += size + 1;
  Set_field(rez, 0, (value)HEADER(size, tag));
  // printf("*rez = 0x%" PRIxVAL "\n", *rez);
  value ans = rez + 1; // +1 because value is a pointer
  // printf("ans = 0x%" PRIxVAL "\n", ans);
  assert(((rukaml_uint_t)ans & (rukaml_word_size - 1)) == 0);
  // printf("TAG(ans) = 0x%" PRIxVAL "\n", TAG(ans));
  assert(TAG(ans) == tag);
  assert(SIZE(ans) == size);
  log("A block 0x%" PRIxVAL " is created of size %ld. Allocated words = %lu\n",
      (value)(rez + 1), size, GC.allocated_words);

  for (size_t i = 0; i < size; ++i)
    Set_field(ans, i, Val_int(0));

  // rukaml_trace_val(ans, 3);
  // fflush(stdout);
  return ans;
}

void *rukaml_alloc_closure(void *func, int32_t argsc) {
  assert(func != NULL);
  assert(argsc > 0);
  // log("%s\n", __func__);
  // code_ptr + argsc + argmax DEBUG+ args[]
  size_t size = 3 + argsc;
  value *ans = (value *)rukaml_alloc_block(size, Closure_tag);
  assert(TAG(ans) == Closure_tag);
  assert(SIZE(ans) == size);
  // printf("%s ans = 0x%" PRIx64 "\n", __func__, (uint64_t*)ans);
  // fflush(stdout);

  Set_clo_code(ans, func);
  Set_clo_arity(ans, Val_int(argsc));
  Set_clo_received(ans, Val_int(0));
#if RUKAML_DEBUG == 1
  // printf("\nstore code ptr = 0x%" PRIxPTR "\n", (Clo_code(ans)));
#endif
  memset(ans + 3, 0, argsc * sizeof(void *));
#if RUKAML_DEBUG == 1
  printf("%s argc = %" PRIdVAL ", ans = 0x%" PRIxPTR "\n\n", __func__,
         Clo_arity(ans), ans);
  fflush(stdout);
#endif
  assert(Clo_arity(ans) == Val_int(argsc));
  assert(Clo_received(ans) == Val_int(0));
  return ans;
}

value copy_closure(value src) {
  assert(IS_BLOCK(src));
  value dst = rukaml_alloc_closure(Clo_code(src), Int_val(Clo_arity(src)));
  assert(TAG(dst) == Closure_tag);
  assert(Clo_arity(dst) == Clo_arity(src));
  Set_clo_received(dst, Clo_received(src));
  assert(Clo_received(dst) == Clo_received(src));
  for (size_t i = 0; i < Int_val(Clo_received(src)); i++) {
    Set_clo_arg(dst, i, Clo_arg(src, i));
  }

  return dst;
}

value rukaml_alloc_array(value size) {
  return rukaml_alloc_block(Int_val(size), Array_tag);
}

// Standart CC
value rukaml_tag0(value obj) {
  assert(obj != NULL);
  return Val_int(TAG(obj));
}

value rukaml_tag(DECLARE_FAKE_ARGS, value obj) { return rukaml_tag0(obj); }

uint64_t rukaml_array_length(DECLARE_FAKE_ARGS, value arr) {
  assert(arr != NULL);
  return SIZE(arr);
}

// read stdin into array, not into string
value rukaml_array_stdin(void) {
  char buf[MAX_STRING_FROM_STDIN];
  int code = scanf("%s", buf);
  if (!code) {
    return rukaml_alloc_array(0);
  }
  size_t size = strlen(buf);
  value arr = rukaml_alloc_array(Val_int(size));

  for (int i = 0; i < size; i++) {
    Set_field(arr, i, Val_int(buf[i]));
  }
  return arr;
}

// read from file
// TODO: recheck
value rukaml_array_read_in(DECLARE_FAKE_ARGS, void **str) {
  int len = 0;
  while (str[len++] != 0)
    ;
  char path[len];
  for (int i = 0; i < len; i++) {
    path[i] = (char)((uint64_t)(str[i]));
  }

  FILE *fp = fopen(path, "r");
  if (!fp) {
    return rukaml_alloc_array(0);
  }

  fseek(fp, 0, SEEK_END);
  size_t size = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  value arr = rukaml_alloc_array(Val_int(size));

  for (int i = 0; i < size; i++) {
    value c = Val_int((uint64_t)fgetc(fp));
    Set_field(arr, i, c);
  }
  fread(arr, sizeof(void *), size, fp);
  fclose(fp);
  return arr;
}

void *rukaml_array_get_sysv(value arr, value n) {
  assert(TAG(arr) == Array_tag);
  if (Int_val(n) >= SIZE(arr)) {
    fprintf(stderr, "Index out of bounds");
    exit(1);
  }
  return (value)(arr[Int_val(n)]);
}

value rukaml_array_get(DECLARE_FAKE_ARGS, value arr, value n) {
  return rukaml_array_get_sysv(arr, n);
}

void rukaml_array_set_sysv(value arr, value n, value a) {
  assert(TAG(arr) == Array_tag);
  if (Int_val(n) >= SIZE(arr)) {
    fprintf(stderr, "Index out of bounds");
    exit(1);
  }
  Set_field(arr, Int_val(n), a);
  // ((value *)arr)[n] = a; // TODO: set_field
  return;
}
void rukaml_array_set(DECLARE_FAKE_ARGS, value arr, value n, value a) {
  return rukaml_array_set_sysv(arr, n, a);
}

value rukaml_field(size_t n, value r) {
  assert(IS_ON_HEAP(r));
  assert(n < SIZE(r));
  // value *arr = (value *)r;
  value ans = Field(r, n);
  if (0) {
    printf("%s: field %" PRIx64 "d = 0x%" PRIxPTR "\n", __func__, n, ans);
    rukaml_trace_val(ans, 3);
  }
  return ans;
}

void *rukaml_applyN(value f, rukaml_int_t argc, ...) {
  assert(IS_ON_HEAP(f));
  assert(TAG(f) == Closure_tag);
  va_list argp;
  va_start(argp, argc);
#if (RUKAML_DEBUG == 1)
  rukaml_trace_val(f, 2);
  log("%s argc = %" PRIx32 ", clo_val = 0x%" PRIxVAL "\n\n", __func__, argc, f);
  fflush(stdout);

  log("%s Clo_code = 0x%" PRIXPTR "\n", __func__, (void *)(Clo_code(f)));
  log("%s Clo_arity = %" PRIdVAL ", Clo_received = %" PRIdVAL "\n\n", __func__,
      Int_val(Clo_arity(f)), Int_val(Clo_received(f)));

  for (size_t i = 0; i < Int_val(Clo_received(f)); ++i) {
    value arg = Clo_arg(f, i);
    if (IS_ON_HEAP(arg)) {
      rukaml_trace_val(arg, 7);
    } else
      log("    Arg %d is not on heap: 0x%" PRIX64 "\n", i, arg);
  }
  fflush(stdout);
#endif
  // printf("f->arg_received = %u\n", f_closure->args_received);
  //  printf("%d\n", __LINE__);
  assert(Int_val(Clo_arity(f)) < 100);
  assert(Clo_received(f) < Clo_arity(f));
  if (Int_val(Clo_received(f)) + argc == Int_val(Clo_arity(f))) {
    // for full application we can omit copying closure
    // log("Full application\n");
    size_t i = 0, j;
    fun0 callable;
    // log("argsc = %d, args_received=%d, new_args=%d\n",
    //     f_closure->argsc, f_closure->args_received, argc);
    void **stack_args = alloca(Int_val(Clo_arity(f)) * sizeof(void *));
    for (i = 0; i < Int_val(Clo_received(f)); ++i) {
      stack_args[i] = Clo_arg(f, i);
      // printf("Setting arg %u: 0x%" PRIxVAL "\n", i, stack_args[i]);
      // fflush(stdout);
      // if (IS_ON_HEAP(stack_args[i]))
      //   rukaml_trace_val(stack_args[i], 3);
      // else
      //   log(" not on heap");
    }

    // rest of the args
    for (j = Int_val(Clo_received(f)); j < Int_val(Clo_arity(f)); j++) {
      stack_args[j] = va_arg(argp, value);
      // printf("Setting arg j=%u, next_arg = 0x%" PRIxVAL "\n", j,
      // stack_args[j]);
    }
    va_end(argp);
    callable = (fun0)(Clo_code(f));
    return callable();
  } else {
    // There we have under application
    value ans_closure = copy_closure(f);

    for (size_t i = 0; i < argc; i++) {
      value arg = (value)va_arg(argp, void *);
      // printf("partial application \n", __LINE__);
      // rukaml_trace_val(arg, 3);
      // fflush(stdout);
      // ans_closure->args[ans_closure->args_received++] = arg;
      size_t received = Int_val(Clo_received(ans_closure));
      Set_clo_arg(ans_closure, received, arg);
      received++;
      Set_clo_received(ans_closure, Val_int(received));
    }
#ifdef DEBUG
    printf("\nf->arg_received = %lu, f->argc = %lu\n",
           ans_closure->args_received, ans_closure->argsc);
    fflush(stdout);
#endif
    va_end(argp);

    return ans_closure;
  }
  __builtin_unreachable();
}

void *rukaml_match_failure() {
  puts("Match failure");
  fflush(stdout);
  exit(1);
}

value rukaml_alloc_string(value len) {
  size_t payload_words_n = (Int_val(len) + 1 + 7) / 8;
  void *block = rukaml_alloc_block(payload_words_n, String_tag);
  assert(TAG(block) == String_tag);
  assert(SIZE(block) == payload_words_n);
  memset(block, '\0', payload_words_n * sizeof(void *));
  // printf("String created at addr = 0x%lX\n", block);

  return block;
}

value rukaml_string_length_sysv(value str) {
  if (str == NULL) {
    mk_err_fatal("unexpected null ptr");
  }

  assert(IS_ON_HEAP(str));
  if (TAG(str) != String_tag) {
    // mk_err_fatal("tag mismatch");
    fprintf(stderr, "[warning] file=%s line=%d msg=tag mismatch, got %lu\n",
            __FILE__, __LINE__, TAG(str));
    fflush(stderr);
    exit(1);
  }

  int64_t ans = strlen((char *)str);
  // printf("Len of rukaml string at addr = 0x%LX is %d\n", str, ans);
  // fflush(stdout);
  return Val_int(ans);
}

void *rukaml_make_string_of_lit(const char *const s) {
  // #ifdef RUKAML_DEBUG
  //   // printf("%s, str = '%s'\n", __func__, s); fflush(stdout);
  //   // pp_string_as_HEX( (char*)s);
  // #endif
  const size_t len = strlen(s);
  value block = (value)rukaml_alloc_string(Val_int(len));

  for (size_t i = 0; i < len; ++i)
    ((char *)(block))[i] = s[i];

  // #ifdef RUKAML_DEBUG
  //   // log("block contents = ", (char*)block);
  //   // pp_string_as_HEX((char*)block);
  //   // log ("\n");
  // #endif

  assert(rukaml_string_length_sysv(block) == Val_int(len));
  // printf("String created at addr = 0x%lX\n", block);
  return (void *)block;
}

value rukaml_string_nth_sysv(value str, value n) {
  assert(str != NULL);
  // log("%s n = %d, str = '%s', \n", __FUNCTION__, n, str);

  if (str == NULL) {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(str) != String_tag) {
    mk_err_fatal("tag mismatch");
  }

  value str_len = rukaml_string_length_sysv(str);
  // log("%s str = '%s', n = %d, strlen = %d\n", __func__, str, n, str_len);

  if (Val_int(n) >= str_len) {
    mk_err_fatal("index out of bounds");
  }

  return Val_int(((char *)str)[Int_val(n)]);
}

void *rukaml_output_string_sysv(int dest, value str) {
  if (dest != 1) {
    log("dest = %ld\n", (int64_t)dest);
    assert(dest == 1);
  };
  // printf("str addr = 0x%lX, str = '%s', len=%d\n", str, str, strlen(str));
  if (TAG(str) != String_tag) {
    printf("str argument = 0x%" PRIx64 ", tag = %ld\n", (long unsigned)str,
           TAG(str));
    mk_err_fatal("tag mismatch");
  }
  printf("%s", (char *)str);
  // puts(str);
  fflush(stdout);
  return 0;
}

// TODO: we are currently printing only to stdout
// Need to fix this.
void rukaml_fprintf_impl(void *dest, value fmt, va_list args) {
  if (fmt == NULL) {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(fmt) != String_tag) {
    mk_err_fatal("tag mismatch");
  }

  value fmt_len = rukaml_string_length_sysv((value)fmt);

  // printf("%s, fmtlen = %ld\n", __func__, fmt_len);
  // fflush(stdout);
  for (size_t pos = 0; pos < Int_val(fmt_len); ++pos) {
    char ch = Int_val(rukaml_string_nth_sysv((value)fmt, Val_int(pos)));
    if (ch != '%') {
      // TODO: fix hardcoded stdout
      putc(ch, stdout);
      if (ch == '\n')
        fflush(stdout);
      continue;
    }
    pos++;

    if (pos >= Int_val(fmt_len)) {
      mk_err_fatal("invalid fmt");
    }

    ch = Int_val(rukaml_string_nth_sysv((value)fmt, Val_int(pos)));

    switch (ch) {
    case 'a': {
      // fflush(stdout);
      void *pp_item_closure = va_arg(args, void *);
      void *item = va_arg(args, void *);
      rukaml_applyN(pp_item_closure, 2, dest, item);
      break;
    }
    case 'b': {
      value v = va_arg(args, value);
      // TODO: fix hardcoded stdout
      fprintf(stdout, "%s", Int_val(v) ? "true" : "false");
      break;
    }
    case 'c': {
      value v = va_arg(args, value);
      // TODO: fix hardcoded stdout
      fprintf(stdout, "%c", (char)Int_val(v));
      break;
    }
    case 'd': {
      int64_t v = va_arg(args, int64_t);
      // printf("%s %d, dest = %d, v=%ld\n", __func__, __LINE__, dest, v);
      fflush(stdout);
      // TODO: fix hardcoded stdout
      fprintf(stdout, "%ld", v);
      break;
    }
    case 's': {
      value str = (value)va_arg(args, void **);
      assert(TAG(str) == String_tag);
      // printf("String case: 0x%lX\n", (uint64_t)str);
      // rukaml_fprintf_string(dest, str);
      // log("%s %d, dest = %d, v=%ld\n", __func__, __LINE__, dest, str);
      // log("%s, stdout = %lx, STDOUT_FILENO = %lx\n", __func__, stdout,
      // STDOUT_FILENO);
      rukaml_output_string_sysv(1, str);
      break;
    }

    case '%': {
      fputc('%', dest);
      break;
    }

    default: {
      mk_err_fatal("invalid fmt");
      break;
    }
    }
  }
}

void *rukaml_fprintf_wrap(DECLARE_FAKE_ARGS, void *out_channel, value fmt,
                          ...) {
  // log("%s, ch=%X, fmt=%" PRIx64 "\n", __func__, out_channel, fmt);
  if (fmt == NULL) {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(fmt) != String_tag) {
    mk_err_fatal("tag mismatch");
  }

  va_list args;
  va_start(args, fmt);
  rukaml_fprintf_impl(out_channel, fmt, args);
  va_end(args);
  return NULL;
}

// arity stands for amount of arguments which fprintf (or other format printer)
// expects after a format string for example, arity for "123" is equal to 0, for
// "%a" is equal to 2, for "%a %s" is equal to 3
uint64_t eval_fmt_arity(value fmt) {
  if (fmt == NULL) {
    mk_err_fatal("unexpected null");
  }
  assert(TAG(fmt) == String_tag);

  uint64_t fmt_len = Int_val(rukaml_string_length_sysv((value)fmt));
  // log("\n%s len = %ld\n", __func__, fmt_len);
  // pp_string_as_HEX(fmt);

  uint64_t arity_acc = 0;

  for (size_t pos = 0; pos < fmt_len; ++pos) {
    char ch = Int_val(rukaml_string_nth_sysv((value)fmt, Val_int(pos)));
    // log("%d: char = '%c'\n", __LINE__, ch);
    if (ch != '%') {
      continue;
    }

    pos++;

    if (pos >= fmt_len) {
      mk_err_fatal("invalid fmt");
    }

    ch = Int_val(rukaml_string_nth_sysv((value)fmt, Val_int(pos)));

    switch (ch) {
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

value rukaml_alloc_fprintf_closure_sysv(void *out_channel, value fmt) {
  if (out_channel == NULL) {
    mk_err_fatal("unexpected null");
  }

  if (fmt == NULL) {
    mk_err_fatal("unexpected null");
  }
  assert(TAG(fmt) == String_tag);

  uint64_t arity = eval_fmt_arity(fmt);
  // log("%s, arity = %lu, fmt = '%s'\n", __func__, arity, (char *)fmt);
  // log("out_channel = %ld\n", (int64_t)out_channel);
  fflush(stdout);
  if (arity == 0) {
    return rukaml_fprintf_wrap(FAKE_ARGS, out_channel, fmt);
  }

  value closure = rukaml_alloc_closure((void *)rukaml_fprintf_wrap, 2 + arity);
  assert(TAG(closure) == Closure_tag);

  return rukaml_applyN(closure, 2, out_channel, fmt);
}

void *rukaml_alloc_fprintf_closure(DECLARE_FAKE_ARGS, value dest, value fmt) {
  assert(TAG(fmt) == String_tag);
  return rukaml_alloc_fprintf_closure_sysv(dest, fmt);
}

void *rukaml_alloc_printf_closure0(value fmt) {
  assert(TAG(fmt) == String_tag);
  return rukaml_alloc_fprintf_closure(FAKE_ARGS, Val_int(1), fmt);
}

// SPRINTF

static void sprintf_insert_arg(value *__args_start, value *__args_fin,
                               value x) {
  value ans = rukaml_alloc_block(2, 0);
  if (*__args_start == NULL) {
    assert(*__args_fin == NULL);
    *__args_start = ans;
    Set_field(*(value **)__args_start, 0, x);
    Set_field(*(value **)__args_start, 1, Val_nil);
    // **__args_start = x;
    *__args_fin = *__args_start + 1;
    *__args_fin = *__args_start; // last/first cons cell
  } else {
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

static size_t eval_int_repr(value _v) {
  // TODO: take sign into account
  rukaml_int_t v = Int_val(_v);
  size_t ans = 0;
  while (true) {
    if (v < 10) {
      ans++;
      break;
    }
    if (v < 100) {
      ans += 2;
      break;
    }
    if (v < 1000) {
      ans += 3;
      break;
    }
    v = v / 1000;
    ans += 3;
  }
  return ans;
}

value rukaml_sprintf_impl(value fmt, va_list args) {
  if (fmt == NULL) {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(fmt) != String_tag) {
    mk_err_fatal("tag mismatch");
  }

  size_t pos;
  size_t fmt_len = Int_val(rukaml_string_length_sysv(fmt));
  size_t output_len = 1; // for '\0'
  // log("%s, fmtlen = %" PRIu64 "\n", __func__, fmt_len);
  value __args_start = NULL;
  value __args_fin = NULL;
  size_t argslen = 0;

  // Collection loop
  for (pos = 0; pos < fmt_len; ++pos) {
    char ch = Int_val(rukaml_string_nth_sysv(fmt, Val_int(pos)));
    // printf("ch = '%c'\n", ch);
    if (ch != '%') {
      output_len++;
      continue;
    }

    pos++;

    if (pos >= fmt_len) {
      mk_err_fatal("invalid fmt");
    }

    ch = Int_val(rukaml_string_nth_sysv(fmt, Val_int(pos)));

    switch (ch) {
    case 'a': {
      value pp_item_closure = va_arg(args, void *);
      value arg1 = va_arg(args, void *);
      value rez = rukaml_applyN(pp_item_closure, 2, Val_unit, arg1);
      argslen++;
      sprintf_insert_arg(&__args_start, &__args_fin, (void *)rez);
      break;
    }
    case 'b': {
      int64_t v = va_arg(args, int64_t);
      if (v == 0)
        output_len += 5;
      else
        output_len += 4;
      argslen++;
      sprintf_insert_arg(&__args_start, &__args_fin, (void *)v);
      break;
    }
    case 'c': {
      char c = (char)va_arg(args, int64_t);
      output_len++;
      argslen++;
      sprintf_insert_arg(&__args_start, &__args_fin, Val_int(c));
      break;
    }
    case 'd': {
      value v = va_arg(args, value);
      if (Int_val(v) < 0)
        output_len++;
      output_len += eval_int_repr(v);
      argslen++;
      sprintf_insert_arg(&__args_start, &__args_fin, v);
      break;
    }
    case 's': {
      value str = (value)va_arg(args, void **);
      // rukaml_trace_val(str, 5);
      sprintf_insert_arg(&__args_start, &__args_fin, str);
      // printf("args_start = 0x%Lx\n", __args_start);
      output_len += Int_val(rukaml_string_length_sysv(str));
      argslen++;
      break;
    }
    case '%': {
      output_len++;
      break;
    }

    default: {
      mk_err_fatal("invalid fmt");
      break;
    }
    }
  }

  // printf("Args are collected, output_len = %lu, args_start = 0x%Lx\n",
  // output_len, __args_start); rukaml_trace_val((value)__args_start, 0);
  // fflush(stdout);
  value ans = rukaml_alloc_string(Val_int(output_len));

  // Main sprintfing loop
  // log("\nMAIN sprintfing loop\n");
  // printf("rez = '%s'\n", (char*) ans);
  // fflush(stdout);

  size_t i = 0;
  for (pos = 0; pos < fmt_len; ++pos) {
    char ch = Int_val(rukaml_string_nth_sysv(fmt, Val_int(pos)));

    if (ch != '%') {
      ((char *)ans)[i] = ch;
      i++;
      continue;
    }

    pos++;

    if (pos >= fmt_len) {
      mk_err_fatal("invalid fmt");
    }

    ch = Int_val(rukaml_string_nth_sysv(fmt, Val_int(pos)));

    switch (ch) {
    case 'b': {
      assert(argslen > 0);
      bool arg = (bool)Field(__args_start, 0);
      if (arg) {
        memcpy((char *)ans + i, "true\0", 1 + 4);
        i += 4;
      } else {
        memcpy((char *)ans + i, "false\0", 1 + 5);
        i += 5;
      }
      __args_start = (value)Field((value *)__args_start, 1);
      break;
    }
    case 'c': {
      char c = (char)va_arg(args, int64_t);
      ans[i] = c;
      i++;
      __args_start = (value)Field((value *)__args_start, 1);
      break;
    }
    case 'd': {
      value arg = Field(__args_start, 0);
      // log("going to print integer %ld\n", arg);
#if __riscv_xlen == 64
      size_t printed = sprintf((char *)ans + i, "%ld", Int_val(arg));
#endif
#if __riscv_xlen == 32
      size_t printed = sprintf((char *)ans + i, "%d", Int_val(arg));
#endif
      i += printed;
      __args_start = (value)Field((value *)__args_start, 1);
      break;
    }
    case 'a':
    case 's': {
      assert(argslen > 0);
      value arg = Field(__args_start, 0);
      // log("Got %%s specifier, __args_start = 0x%lx, arg = 0x%lx, arg =
      // '%s'\n", __args_start, arg, (char*)arg);
      assert(TAG(arg) == String_tag);
      size_t l = strlen((char *)arg);
      memcpy((char *)ans + i, (char *)arg, l);
      i += l;
      argslen--;
      __args_start = (value)Field((value *)__args_start, 1);
      // printf("New args_start = %LX\n", __args_start); fflush(stdout);
      break;
    }

    case '%': {
      ((char *)ans)[i] = '%';
      i++;
      break;
    }

    default: {
      mk_err_fatal("invalid fmt");
      break;
    }
    }
  }

  // printf("Sprintf finished, output_len = %lu, rez = '%s'\n", output_len,
  // (char*) ans); pp_string_as_HEX((char*)ans);
  assert(TAG(ans) == String_tag);
  return ans;
}

value rukaml_sprintf_wrap(DECLARE_FAKE_ARGS, value fmt, ...) {
  if (fmt == NULL) {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(fmt) != String_tag) {
    mk_err_fatal("tag mismatch");
  }

  va_list args;
  va_start(args, fmt);
  value ans = rukaml_sprintf_impl(fmt, args);
  va_end(args);

  // uint64_t payload_words_n = (bytes_n + 7) / 8;                     // amount
  // of (64-bits) words req void **obj = rukaml_alloc_block(payload_words_n + 1,
  // String_tag); // +1 stands for additional word obj[payload_words_n] = (void
  // *)bytes_n;                           // stores String.length

  // uint64_t n = fread((void *)obj, sizeof(char), bytes_n, tmp);

  return ans;
}
//
void *rukaml_alloc_sprintf_closure(DECLARE_FAKE_ARGS, value fmt) {
  if (fmt == NULL) {
    mk_err_fatal("unexpected null");
  }

  if (TAG(fmt) != String_tag) {
    mk_err_fatal("tag mismatch");
  }

  uint64_t arity = eval_fmt_arity(fmt);

  if (arity == 0) {
    return rukaml_sprintf_wrap(FAKE_ARGS, fmt);
  }

  void *closure = rukaml_alloc_closure(rukaml_sprintf_wrap, 1 + arity);

  return rukaml_applyN(closure, 1, fmt);
}

value rukaml_alloc_sprintf_closure_sysv(value fmt) {
  assert(TAG(fmt) == String_tag);
  return rukaml_alloc_sprintf_closure(FAKE_ARGS, fmt);
}

// rukaml polymorphic equality for SysV ABI
value rukaml_equal_sysv(value l, value r) {
  if (l == r) {
    return Val_true;
  }

  if (IS_ON_HEAP(l) && IS_ON_HEAP(r)) {
    if (TAG(l) != TAG(r))
      return Val_false;
    if (TAG(l) == String_tag) {
      int ans = strcmp((char *)l, (char *)r);
      return (ans == 0) ? Val_true : Val_false;
    }
    assert(SIZE(l) == SIZE(r));
    if (SIZE(l) == 0)
      return Val_true;
    for (size_t i = 0; i < SIZE(l); i++) {
      value lf = Field(l, i);
      value rf = Field(r, i);
      if (Val_false == rukaml_equal_sysv(lf, rf)) {
        return Val_false;
      }
    }
    return Val_true;
  }

  return Val_false;
}

value rukaml_equal(DECLARE_FAKE_ARGS, value l, value r) {
  return rukaml_equal_sysv(l, r);
}

value rukaml_compare_sysv(value l, value r) {
  // printf("%s. %ld vs %ld\n", __func__, l, r);
  if (l == r) {
    // printf("%s %d\n", __func__, __LINE__);
    fflush(stdout);
    return Val_int(0);
  }

  if (IS_ON_HEAP(l) && IS_ON_HEAP(r)) {
    // printf("Got two heap values\n");
    // fflush(stdout);
    if (TAG(l) < TAG(r))
      return Val_int(-1);
    if (TAG(l) > TAG(r))
      return Val_int(1);
    if (TAG(l) == String_tag) {
      int ans = strcmp((char *)l, (char *)r);
      if (ans < 0)
        return Val_int(-1);
      if (ans == 0)
        return Val_int(0);
      return Val_int(1);
    }
    assert(SIZE(l) == SIZE(r));
    if (SIZE(l) == 0)
      return Val_int(0);
    for (size_t i = 0; i < SIZE(l); i++) {
      value lf = Field(l, i);
      value rf = Field(r, i);
      value ans = rukaml_equal_sysv(lf, rf);
      if (ans != Val_int(0)) {
        return ans;
      }
    }
    return Val_int(0);
  }

  if (!IS_ON_HEAP(l) && !IS_ON_HEAP(r)) {
    // printf("Got two immediates\n");
    // fflush(stdout);
    if (Int_val(l) < Int_val(r))
      return Val_int(-1);
    if (Int_val(l) == Int_val(r))
      return Val_int(0);
    return Val_int(1);
  }
  printf("other\n");
  fflush(stdout);
  if (!IS_ON_HEAP(l))
    return Val_int(-1);
  return Val_int(1);
}

value rukaml_compare(DECLARE_FAKE_ARGS, value l, value r) {
  return rukaml_compare_sysv(l, r);
}

value rukaml_sys_exit_sysv(value n) {
  exit(Int_val(n));
  __builtin_unreachable();
}

value rukaml_list_length(DECLARE_FAKE_ARGS, value ls) {
  for (size_t size = 0;; ++size) {
    assert(IS_BLOCK(ls));
    switch (TAG(ls)) {
    case 1: // cons
      assert(2 == SIZE(ls));
      ls = Field(ls, 1);
      break;
    case 0:
      return Val_int(size);
    default:
      mk_err_fatal("tag mismatch");
    }
  }
}

value rukaml_string_of_char_list_sysv(value chs) {
  assert(IS_BLOCK(chs));
  value chars_n = rukaml_list_length(FAKE_ARGS, chs);
  const size_t n = (size_t)Int_val(chars_n);
  // uint64_t payload_words_n = (chars_n + 7) / 8;
  // printf("%s, charsN = %ld\n", __func__, n );
  value block = rukaml_alloc_string(chars_n + 1);

  assert(rukaml_string_length_sysv(block) == 0);

  char *repr = (char *)block;
  repr[n] = '\0';
  for (size_t i = 0; i < n; ++i) {
    assert(TAG(chs) == 1);                    // tag of ( :: )
    repr[i] = (char)Int_val((Field(chs, 0))); // get head
    chs = Field(chs, 1);                      // get next list node
  }

  // printf("result is '%s' of len = %lu\n", repr, strlen(repr));
  // fflush(stdout);
  assert(Int_val(strlen(repr)) == Int_val(rukaml_string_length_sysv(block)));

  return block;
}

value rukaml_string_of_char_list(DECLARE_FAKE_ARGS, value chs) {
  return rukaml_string_of_char_list_sysv(chs);
}

value rukaml_substring_sysv(value _str, value _from, value _len) {
  assert(IS_BLOCK(_str));
  assert(TAG(_str) == String_tag);
  char *s = (char *)_str;
  size_t slen = strlen(s);
  assert(Int_val(_from) + Int_val(_len) <= slen);
  value _ans = rukaml_alloc_string(_len);
  memcpy((char *)_ans, s + Int_val(_from), Int_val(_len));
  return _ans;
}

value rukaml_end_of_input_sysv(value channel) {
  assert(Int_val(channel) == 0); // stdin
  int ch = fgetc(stdin);

  if (ch == EOF) {
    return Val_true;
  }

  ungetc(ch, stdin);

  return Val_false;
}
value rukaml_end_of_input(DECLARE_FAKE_ARGS, value channel) {
  return rukaml_end_of_input_sysv(channel);
}
void *rukaml_open_impl(value path, const char *mode) {
  if (path == NULL) {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(path) != String_tag) {
    mk_err_fatal("tag mismatch");
  }

  if (mode == NULL) {
    mk_err_fatal("unexpected null ptr");
  }

  size_t len = (size_t)Int_val(rukaml_string_length_sysv(path));

  char *path_cstr = malloc(len + 1);

  if (path_cstr == NULL) {
    mk_err_fatal("memory allocation failed");
  }

  memcpy(path_cstr, (const void *)path, len);

  path_cstr[len] = '\0';

  FILE *file = fopen(path_cstr, mode);

  if (file == NULL) {
    mk_err_fatal("fopen failed");
  }

  free(path_cstr);

  return (void *)file;
}

void *rukaml_open_in_sysv(value path) {
  // printf("%s. file = %s\n", __func__, (char *)path);
  return rukaml_open_impl(path, "r");
}

void *rukaml_open_in(DECLARE_FAKE_ARGS, value path) {
  // printf("%s. file = %s\n", __func__, (char *)path);
  return rukaml_open_impl(path, "r");
}

value rukaml_open_out(DECLARE_FAKE_ARGS, value path) {
  return rukaml_open_impl(path, "w");
}

void rukaml_close_channel(DECLARE_FAKE_ARGS, void *channel) {
  if (channel == NULL) {
    mk_err_warning("unexpected null");
  } else {
    fclose((FILE *)channel);
  }
}

value rukaml_input_all_sysv(value _file) {
  if (_file == 0) {
    printf("Can't read from stdin\n");
    exit(1);
  }
  if (_file == 1) {
    printf("Can't read from stdout\n");
    exit(1);
  }
  // printf("file = %ld\n", _file);
  FILE *file = (FILE *)_file;
  fseek(file, 0, SEEK_END);
  size_t size = ftell(file);
  fseek(file, 0, SEEK_SET);

  // printf("size = %ld\n", size);
  // fflush(stdout);
  value ans = rukaml_alloc_string(Val_int(size));

  fread((char *)ans, 1, size, file);
  return ans;
}
