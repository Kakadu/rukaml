#include <assert.h>
#include <alloca.h>
#include <errno.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
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


static uint64_t log_level = 0x0;

#ifdef RUKAML_DEBUG
#define log(...)       \
  if (1) \
  {printf(__VA_ARGS__); fflush(stdout); }
#else
#warning "DEBUG is not enabled"
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


int HEAP_SIZE = 16384 ; // in words
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

// 0x55555565F2A0
// 0x55555555F2A0

// 0x2AAAAB578010
// 0x2AAAAB478010
static struct gc_data GC = {.ebp = 0, .allocated_words = 0, .stats = {.gs_allocated_words = 0}};

void rukaml_initialize(uint64_t ebp)
{
  // setbuf(stdout, NULL);
  // {
  //   char *env = getenv("RUKAMLRUNPARAM");
  //   if (env)
  //   {
  //     char *temp;
  //     temp = strtok(env, ",");
  //     while (temp != NULL)
  //     {
  //       if (strlen(temp) <= 2 || temp[1] != '=')
  //         continue;

  //       switch (temp[0])
  //       {
  //       case 'v':
  //       {
  //         uint64_t v = strtol(temp + 2, (char **)NULL, 10);
  //         log_level = v;
  //         break;
  //       }
  //       case 'm':
  //       {
  //         long v = strtol(temp + 2, (char **)NULL, 10);
  //         assert(v > 0);
  //         log("Setting heap size to be %ld words\n", v);
  //         HEAP_SIZE = (uint32_t)v;
  //         break;
  //       }
  //       default:
  //         fprintf(stderr, "Unrecongnized env switch\n");
  //         break;
  //       }
  //       temp = strtok(NULL, ",");
  //     }
  //   }
  // }
  GC.ebp = ebp;
  log("%s. stack_start = 0x%lX\n", __func__, GC.ebp);
  const uint64_t size = sizeof(uint64_t *) * HEAP_SIZE;
  printf("Malloc heap size = %ld (0x%LX)\n", size, size);
  GC.main_bank = malloc(size);
  memset(GC.main_bank, 0x80, size);
  GC.main_bank_fin = GC.main_bank +   HEAP_SIZE;
  GC.backup_bank = malloc(sizeof(uint64_t *) * HEAP_SIZE);
  GC.backup_bank_fin = GC.backup_bank +   HEAP_SIZE;
  GC.allocated_words = 0;
  GC.stats.gs_current_bank = 0;
  printf("main   bank: 0x%lX..0x%lX\n", (uint64_t)GC.main_bank, (uint64_t)GC.main_bank_fin);
  printf("backup bank: 0x%lX..0x%lX\n", (uint64_t)GC.backup_bank, (uint64_t)GC.backup_bank_fin);
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
  assert(false);
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

typedef struct
{
  void *code;
  int64_t argsc; // TODO(Kakadu): uint32_t or byte ???
  int64_t args_received;
  void *args[0];
} rukaml_closure;



#define PAD(n) \
  { for (unsigned int i = 0; i < n; i++) \
      printf(" "); }
void rukaml_trace_val(void *arg, unsigned int level)
{
  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wunused-parameter"
  #pragma GCC diagnostic ignored "-Wpointer-to-int-cast"

  PAD(level);
  printf("BLOCK:  0x%" PRIx64 ", he=0x%LX, tag=%lu, size=%lu" "\n",
    (uint64_t)arg, ((uint64_t*)arg)[-1], TAG(arg), SIZE(arg));
  if (TAG(arg) == String_tag) {
    PAD(level + 1);
    printf("\"%s\"\n", (char*)arg);
    return;
  }
  if (TAG(arg) == Closure_tag) {
    PAD(level + 1);
    rukaml_closure *clo = (rukaml_closure*) arg;
    printf("closure, code = 0x%" PRIx64 ", argc=%d, arg_got=%d\n",
      clo->code, clo->argsc, clo->args_received);
    return;
  }
  // Need to implement tagged integers
  for (uint64_t i = 0; i < SIZE(arg); i++)
  {
    void **field = (void**) FIELD(arg, i);
    if ((unsigned)(*field) < 100) {
      PAD(level + 1);
      printf("%lu -> Int %ld\n", i, (int64_t)(*field));
    }
    else {
      PAD(level + 1);
      printf("%lu -> %d ", i);
      rukaml_trace_val(*field, level  + 1);

    }
  }
  fflush(stdout);
  #pragma GCC diagnostic pop
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

void* rukaml_print_newline_sysv() {
  printf("\n");
  return 0;
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
  log("A pair %lX created. Allocated words = %lu\n", (uint64_t)(rez + 1), GC.allocated_words);
  return rez + 1;
}
//0x55555555F2A0 - 0x55555565F2A0
void *rukaml_alloc_block(int64_t size, uint8_t tag)
{
  static size_t call_count = 0;
  call_count++;

  if (GC.allocated_words + size + 1 > HEAP_SIZE)
  {
    fprintf(stderr, "Not enough memory\n");
    exit(1);
  }
  uint64_t *rez = ((uint64_t **)(GC.main_bank + GC.allocated_words ));
  assert(is_old_bank(rez));
  printf("call_count = %d\n", call_count);
  // if (call_count >= 311) {
    // printf("GOING TO CRASH\n");
    printf("main_bank = 0x%LX, rez = 0x%LX, bank_fin = 0x%LX, size = %ld\n", GC.main_bank, rez, GC.main_bank_fin, size);
  // }

  GC.allocated_words += size + 1;
  GC.stats.gs_allocated_words += size + 1;
  // if (call_count >= 318) {
  //   assert( GC.main_bank < rez);
  //   assert( rez < GC.main_bank_fin);
  //   printf("%s %d\n", __func__, __LINE__);
  //   printf("rez[0] = %LX\n", rez[0]);
    printf("%s %d\n", __func__, __LINE__);
  //   printf("rez[1] = %LX\n", rez[1]);
  // }
  rez[0] = (uint64_t *)HEADER(size, tag);
  // if (call_count >= 318)
    printf("%s %d\n", __func__, __LINE__);
  uint64_t *ans = &rez[1];
  assert(TAG(ans) == tag);
  assert(SIZE(ans) == size);
  mprotect(ans - 8, 8, PROT_READ);

  log("A block 0x%" PRIx64 " is created.(he=0x%X,tag=%d, size=%d) Allocated words = %lu\n",
        (uint64_t)ans, rez[0], tag, size,
        GC.allocated_words);
  // printf("Header addr = %lX\n", rez);
  return ans;
}

void *rukaml_alloc_array(int64_t size)
{
  return rukaml_alloc_block(size, Array_tag);
}

void *rukaml_alloc_closure(void *func, int32_t argsc)
{
  assert(func != NULL);
  assert(argsc > 0);
  log("%s\n", __func__);
  // code_ptr + argsc + argmax + args[]
  size_t size = 3 + argsc;
  rukaml_closure * ans = (rukaml_closure*)rukaml_alloc_block(size, Closure_tag);
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

rukaml_closure *copy_closure(rukaml_closure *src)
{
  size_t size = sizeof(rukaml_closure) + sizeof(void *) * src->argsc;
  rukaml_closure *dst = (rukaml_closure *)rukaml_alloc_closure(src->code, src->argsc);
  dst->args_received = src->args_received;
  for (size_t i=0; i<src->args_received; i++) {
    dst->args[i] = src->args[i];
  }
  assert(TAG(dst) == Closure_tag);
  assert(dst->argsc == src->argsc);
  return dst;
}

// Standart CC
void *rukaml_tag0(void **obj) {
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
                            char *str) {
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
void *rukaml_array_get_sysv(void** arr, uint64_t n)
{
  return rukaml_array_get(1,2,3,4,5,6,7,8,arr,n);
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

void *rukaml_array_set_sysv(void** arg0, uint64_t arg1, void* arg2)
{
  rukaml_array_set(1,2,3,4,5,6,7,8,arg0,arg1,arg2);
  return 0;
}

void *rukaml_field(int n, void **r)
{
  assert(r != NULL);
  return r[n];
}


void *rukaml_applyN(void *f, int64_t argc, ...)
{
  log("%s, f = 0x%" PRIx64 ", argc=%u\n", __func__, f, argc);
  assert(IS_ON_HEAP(f));
  assert(TAG(f) == Closure_tag);
  // rukaml_trace_val(f, 0);

  va_list argp;
  va_start(argp, argc);
  rukaml_closure *f_closure = f;

  assert(f_closure->args_received + argc <= f_closure->argsc);

  if (f_closure->args_received + argc == f_closure->argsc) {
    size_t i = 0, j;
    fun0 callable;
    log("argsc = %d, args_received=%d, new_args=%d\n",
      f_closure->argsc, f_closure->args_received, argc);
    // for full application we can omit copying closure
    void** stack_args = alloca(f_closure->argsc * sizeof(void*));
    for (i=0; i<f_closure->args_received; ++i) {
      // log("Setting arg %d\n", i);
      stack_args[i] = f_closure->args[i];
    }

    // rest of the args
    for (j = f_closure->args_received; j < f_closure->argsc; j++) {
      // printf("Setting arg j=%d, f_closure->args_received+argc=%d\n", j, f_closure->args_received+argc);
      stack_args[j] = va_arg(argp, void *);
    }

    va_end(argp);

    callable = (fun0)f_closure->code;
    log("Do a call\n");
    return callable();
  } else {
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
  __builtin_unreachable();
}

void *rukaml_match_failure()
{
  puts("Match failure");
  fflush(stdout);
  exit(1);
}

uint64_t rukaml_list_length(int r0, int r1, int r2, int r3, int r4, int r5, void **ls)
{
  printf("%s, lst = 0x%LX\n", __func__, ls);
  for (uint64_t size = 0;; ++size)
  {
    assert(ls != NULL);
    if (TAG(ls) == 0) // [] lowered to int. TODO: adjust for tagged ints
    {
      printf("%s returns %lu\n", __func__, size);
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

  // uint64_t ans = (uint64_t)(str[SIZE(str) - 1]);
  uint64_t ans = strlen((char*)str);
  // log("Len of rukaml string '%s' is %d\n", str, ans);
  return ans;
}

char rukaml_string_nth_sysv(void **str, uint64_t n)
{
  // puts("HERR\n");
  assert(str != NULL);
  // printf("%s n = %d, str = '%s', \n", __FUNCTION__, n, str);
  if (str == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(str) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  uint64_t str_len = 0 ;
  str_len  = rukaml_string_len_imm(str);
  // log("%s str = '%s', n = %d, strlen = %d\n", __func__, str, n, str_len);

  if (n >= str_len)
  {
    mk_err_fatal("index out of bounds");
  }

  return ((char *)str)[n];
}

char rukaml_string_nth(int r0, int r1, int r2, int r3, int r4, int r5, void **str, uint64_t n)
{
  return rukaml_string_nth_sysv(str, n);
}


void* rukaml_make_string_of_lit(const char* const s)
{
  log("%s, str = '%s'\n", __func__, s); fflush(stdout);
  const size_t len = strlen(s);
  size_t payload_words_n = (len + 1 + 7) / 8;
  void** block = (void*)rukaml_alloc_block(payload_words_n, String_tag);
  // printf("String created at addr = 0x%lX\n", block);
  assert(TAG(block) == String_tag);
  assert(SIZE(block) == payload_words_n);
  // block[payload_words_n] = len;
  memset(block, '\0', payload_words_n * sizeof(void*) );
  for (size_t i = 0; i < len; ++i)
    ((char *)(block))[i] = s[i];

  log("block contents = '%s'\n", (char*)block);
  assert(rukaml_string_len_imm((void **)block) == len);
  // printf("String created at addr = 0x%lX\n", block);
  return (void*)block;
}

uint64_t* rukaml_string_of_int_sysv(uint64_t num) {
  log("%s. num = %" PRIu64 "\n", __func__, num);
  int len = snprintf(NULL, 0, "%d", num);
  char *str = malloc(len + 1);
  memset(str, '\0', len+1);
  snprintf(str, len + 1, "%d", num);

  uint64_t* ans = rukaml_make_string_of_lit(str);
  assert(TAG(ans) == String_tag);
  free(str);
  return ans;
}

void **rukaml_string_of_char_list(int r0, int r1, int r2, int r3, int r4, int r5, void **chs)
{
  assert(chs != NULL);
  // TODO: change to Balyshev-style representaiton
  // chs == 0 means [] lowered to int. TODO: adjust for tagged ints
  uint64_t chars_n = 0;
  if (TAG(chs) != 0) {
    chars_n = rukaml_list_length(0, 0, 0, 0, 0, 0, chs);
  }
  uint64_t payload_words_n = (chars_n + 1 + 7) / 8;

  uint64_t *block = (uint64_t *)rukaml_alloc_block(payload_words_n + 1, String_tag);

  memset(block, '\0', payload_words_n);
  for (size_t n = 0; n < chars_n; ++n)
  {
    assert(TAG(chs) == 1); // tag of ( :: )
    ((char *)(block))[n] = (char)(chs[0]);
    chs = (void **)(chs[1]);
  }

  assert(rukaml_string_len_imm((void **)block) == chars_n);
  return (void **)block;
}

void** rukaml_string_of_char_list_sysv(void** chs) {
  return rukaml_string_of_char_list(1,2,3,4,5,6,chs);
}

void* rukaml_output_string_sysv(int dest, void* str)
{
  assert(dest == STDOUT_FILENO);
  // printf("str addr = 0x%lX, str = '%s', len=%d\n", str, str, strlen(str));
  if (TAG(str) != String_tag)
  {
    printf("str argument = 0x%" PRIx64", tag = %d\n", str, TAG(str));
    mk_err_fatal("tag mismatch");
  }
  printf("%s", str);
  // puts(str); //fflush(stdout);
  return 0;
}

void* rukaml_output_int_sysv(int dest, int64_t n)
{
  assert(dest == STDOUT_FILENO);
  printf("%d", n); //fflush(stdout);
  return 0;
}

void* rukaml_output_char_sysv(int dest, char c)
{
  assert(dest == STDOUT_FILENO);
  printf("%c", c);
  // putchar(c); //fflush(stdout);
  return 0;
}

// fprintf stuff below

#if defined(__riscv) && __riscv_xlen == 64
  #define DECLARE_FAKE_ARGS int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7
  #define FAKE_ARGS 0,1,2,3,4,5,6,7
#endif

void rukaml_fprintf_impl(void *dest, void **fmt, va_list args)
{
  // if (dest == NULL)
  // {
  //   mk_err_fatal("unexpected null ptr");
  // }

  if (fmt == NULL)
  {
    mk_err_fatal("unexpected null ptr");
  }

  if (TAG(fmt) != String_tag)
  {
    mk_err_fatal("tag mismatch");
  }

  uint64_t fmt_len = rukaml_string_len_imm(fmt);

  // log("%s, fmtlen = %d\n", __func__, fmt_len);
  for (size_t pos = 0; pos < fmt_len; ++pos)
  {
    char ch = rukaml_string_nth_sysv(fmt, pos);
    // printf("ch = '%c'\n", ch);
    if (ch != '%')
    {
      // TODO: fix hardcoded stdout
      putc(ch, stdout);
      // log("continue\n");
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
      void **str = va_arg(args, void **);
      // rukaml_fprintf_string(dest, str);
      // log("%s %d, dest = %d, v=%ld\n", __func__, __LINE__, dest, str);
      // log("%s, stdout = %lx, STDOUT_FILENO = %lx\n", __func__, stdout, STDOUT_FILENO);
      rukaml_output_string_sysv(STDOUT_FILENO, str);
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
  // log("%s, ch=%X, fmt=%" PRIx64"\n", __func__, out_channel, fmt);
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
  rukaml_fprintf_impl( out_channel, fmt, args);
  va_end(args);
  return NULL;
}

void pp_string_as_HEX(char* str) {
  printf("HEX_string:\n");
  for (size_t i=0; i< strlen(str); ++i) {
    switch(str[i]) {
    case '\n':
      printf(" \\n %X", (unsigned) str[i]);
      break;
    case ' ':
      printf(" ' ' %X", (unsigned) str[i]);
      break;
    default:
      printf("  %c 0x%X", str[i], (unsigned) str[i]);
    }
  }

  printf("\n");
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


  uint64_t fmt_len = rukaml_string_len_imm(fmt);
  log("\n%s len = %d\n", __func__, fmt_len);
  // pp_string_as_HEX(fmt);

  uint64_t arity_acc = 0;

  for (size_t pos = 0; pos < fmt_len; ++pos)
  {
    char ch = rukaml_string_nth_sysv(fmt, pos);

    if (ch != '%')
    {
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


void *rukaml_alloc_fprintf_closure(DECLARE_FAKE_ARGS, void *out_channel, void **fmt)
{
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
  log("%s, arity = %lu\n", __func__, arity);
  if (arity == 0)
  {
    return rukaml_fprintf_wrap(FAKE_ARGS, out_channel, fmt);
  }

  void *closure = rukaml_alloc_closure(rukaml_fprintf_wrap, 2 + arity);

  return rukaml_applyN(closure, 2, out_channel, fmt);
}

void *rukaml_alloc_fprintf_closure0(int dest, void **fmt)
{
  assert(TAG(fmt) == String_tag);
  return rukaml_alloc_fprintf_closure(FAKE_ARGS, dest, fmt);
}

void *rukaml_alloc_printf_closure0(void **fmt)
{
  assert(TAG(fmt) == String_tag);
  return rukaml_alloc_fprintf_closure(FAKE_ARGS, stdout, fmt);
}

void *rukaml_alloc_printf_closure(DECLARE_FAKE_ARGS, void **fmt)
{

  if (fmt == NULL)
  {
    mk_err_fatal("unexpected null fmt");
  }
  assert(TAG(fmt) == String_tag);
  return rukaml_alloc_fprintf_closure(FAKE_ARGS, stdout, fmt);
}
