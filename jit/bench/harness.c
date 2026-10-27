/*
 * jit/bench/harness.c -- online JIT vs rukaml AOT.
 *
 * Matrix:
 *   jit            driver --target jit --opt=online
 *   aot llvm -O3   driver --target llvm, clang -O3 (baseline)
 *   aot rv64       always qemu-riscv64
 *   aot rv32       always qemu-riscv32, when the toolchain exists
 *
 * Offline JIT modes and the OCaml comparison rows are intentionally gone:
 * they made the old harness large and answered a different question.
 *
 * Measurements kept from the old harness:
 *   cold       whole child lifetime at the base size
 *   steady     T(N) = a + b*N over several sizes
 *   overhead   JIT minus AOT-LLVM-O3: cold, fit intercept, fit slope,
 *              empty-main startup, compile probe, online tier-up count
 *   insn       qemu guest instruction count I(N) = a + b*N
 *   results    every checked row must return the baseline's exit status
 *
 * Build: cc -O2 -std=c11 -Wall -Wextra -o jit/bench/harness jit/bench/harness.c
 */

#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L

#include <dirent.h>
#include <errno.h>
#include <sched.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/personality.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define MAX_ROWS 64
#define MAX_REPS 64
#define MAX_SIZES 8
#define MAX_ARGV 32

typedef enum { RUN_HOST, RUN_QEMU64, RUN_QEMU32 } runner;
typedef enum { ROLE_MAIN, ROLE_STARTUP, ROLE_PROBE } role;

typedef struct {
  const char *label;
  role role;
  long n;
  runner runner;
  char *argv[MAX_ARGV];
  int enabled;
  int check_exit;
  int is_qemu;
  int exit_code;
  int nreps;
  double wall[MAX_REPS];
  double cpu[MAX_REPS];
  double min_ms, median_ms, mad_ms, cpu_ms;
  unsigned long long insn;
  int insn_ok;
} row;

static row g_rows[MAX_ROWS];
static int g_nrows;
static const char *g_labels[MAX_ROWS];
static int g_nlabels;
static const char *g_skips[MAX_ROWS];
static int g_nskips;
static const char *g_dir;
static const char *g_log;
static const char *g_driver;
static const char *g_plugin;
static const char *g_only;
static const char *g_skip;
static const char *g_backend_excludes[MAX_ROWS];
static int g_nbackend_excludes;
static long g_base_n;
static long g_sizes[MAX_SIZES];
static int g_nsizes;
static int g_reps = 5;
static int g_cpu = -1;
static int g_qemu_insn;
static int g_verbose;
static int g_tier_ups = -1;

/* --- strings, time, commands --------------------------------------------- */

static char *xvasprintf(const char *fmt, va_list ap) {
  char *s = NULL;
  if (vasprintf(&s, fmt, ap) < 0) {
    perror("vasprintf");
    exit(2);
  }
  return s;
}

static char *xasprintf(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  char *s = xvasprintf(fmt, ap);
  va_end(ap);
  return s;
}

static const char *env_or(const char *name, const char *def) {
  const char *v = getenv(name);
  return (v != NULL && *v != '\0') ? v : def;
}

static void add_backend_excludes(const char *list) {
  const char *p = list;
  while (*p != '\0') {
    while (*p == ',' || *p == ' ' || *p == '\t') ++p;
    if (*p == '\0') break;
    const char *start = p;
    while (*p != '\0' && *p != ',' && *p != ' ' && *p != '\t') ++p;
    if (g_nbackend_excludes < MAX_ROWS)
      g_backend_excludes[g_nbackend_excludes++] =
        xasprintf("%.*s", (int)(p - start), start);
  }
}

static double now_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (double)ts.tv_sec * 1000.0 + (double)ts.tv_nsec / 1e6;
}

static double pct(double delta, double base) {
  return base == 0.0 ? 0.0 : 100.0 * delta / base;
}

static int tool_ok(const char *tool) {
  if (tool == NULL || *tool == '\0') {
    return 1;
  }
  if (strchr(tool, '/') != NULL) {
    return access(tool, X_OK) == 0;
  }
  char *cmd = xasprintf("command -v %s >/dev/null 2>&1", tool);
  int st = system(cmd);
  free(cmd);
  return st != -1 && WIFEXITED(st) && WEXITSTATUS(st) == 0;
}

static int run_cmd_va(int quiet, const char *fmt, va_list ap) {
  char *cmd = xvasprintf(fmt, ap);
  char *full = g_log == NULL ? cmd : xasprintf("%s >>%s 2>&1", cmd, g_log);
  int st = system(full);
  int ok = st != -1 && WIFEXITED(st) && WEXITSTATUS(st) == 0;
  if (!ok && !quiet) {
    fprintf(stderr, "bench: command failed: %s\n", cmd);
    if (g_log != NULL) fprintf(stderr, "       log: %s\n", g_log);
  }
  if (full != cmd) free(full);
  free(cmd);
  return ok;
}

static int run_cmd(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int ok = run_cmd_va(0, fmt, ap);
  va_end(ap);
  return ok;
}

static int run_cmd_quiet(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int ok = run_cmd_va(1, fmt, ap);
  va_end(ap);
  return ok;
}

static int spawn(char *const av[], struct rusage *ru) {
  pid_t pid = fork();
  if (pid < 0) {
    perror("fork");
    exit(2);
  }
  if (pid == 0) {
    if (g_cpu >= 0) {
      cpu_set_t set;
      CPU_ZERO(&set);
      CPU_SET(g_cpu, &set);
      (void)sched_setaffinity(0, sizeof set, &set);
    }
    (void)personality(ADDR_NO_RANDOMIZE);
    execvp(av[0], av);
    fprintf(stderr, "bench: cannot run %s: %s\n", av[0], strerror(errno));
    _exit(127);
  }
  int status = 0;
  struct rusage r;
  memset(&r, 0, sizeof r);
  if (wait4(pid, &status, 0, &r) < 0) {
    perror("wait4");
    exit(2);
  }
  if (ru != NULL) {
    *ru = r;
  }
  return WIFSIGNALED(status) ? -1 : WEXITSTATUS(status);
}

/* qemu rows need a larger guest heap/stack: the riscv runtimes do not collect. */
static int spawn_row(const row *r, char *const av[], struct rusage *ru) {
  int qemu = r != NULL && r->is_qemu;
  char *old_run = NULL, *old_stack = NULL;
  if (qemu) {
    const char *v = getenv("RUKAMLRUNPARAM");
    if (v != NULL) old_run = strdup(v);
    v = getenv("QEMU_STACK_SIZE");
    if (v != NULL) old_stack = strdup(v);
    setenv("RUKAMLRUNPARAM", "m=134217728", 1);
    setenv("QEMU_STACK_SIZE", "536870912", 1);
  }
  int code = spawn(av, ru);
  if (qemu) {
    if (old_run != NULL) { setenv("RUKAMLRUNPARAM", old_run, 1); free(old_run); }
    else unsetenv("RUKAMLRUNPARAM");
    if (old_stack != NULL) { setenv("QEMU_STACK_SIZE", old_stack, 1); free(old_stack); }
    else unsetenv("QEMU_STACK_SIZE");
  }
  return code;
}

/* --- rows and selections ------------------------------------------------- */

static const char *runner_name(runner r) {
  return r == RUN_QEMU64 ? "qemu-rv64" : r == RUN_QEMU32 ? "qemu-rv32" : "host";
}

static const char *role_name(role r) {
  return r == ROLE_STARTUP ? "startup" : r == ROLE_PROBE ? "probe" : "main";
}

static int selected(const char *label) {
  if (g_only != NULL && strstr(label, g_only) == NULL) return 0;
  if (g_skip != NULL && strstr(label, g_skip) != NULL) return 0;
  for (int i = 0; i < g_nbackend_excludes; ++i)
    if (strstr(label, g_backend_excludes[i]) != NULL) return 0;
  return 1;
}

static void note_skip(const char *label, const char *why) {
  char *msg = xasprintf("%s (%s)", label, why);
  for (int i = 0; i < g_nskips; ++i) {
    if (strcmp(g_skips[i], msg) == 0) { free(msg); return; }
  }
  if (g_nskips < MAX_ROWS) g_skips[g_nskips++] = msg;
  else free(msg);
}

static void add_label(const char *label) {
  for (int i = 0; i < g_nlabels; ++i)
    if (strcmp(g_labels[i], label) == 0) return;
  if (g_nlabels < MAX_ROWS) g_labels[g_nlabels++] = label;
}

static row *new_row(const char *label, role r, long n, runner run) {
  if (g_nrows >= MAX_ROWS) {
    fprintf(stderr, "bench: too many rows\n");
    exit(2);
  }
  row *x = &g_rows[g_nrows++];
  memset(x, 0, sizeof *x);
  x->label = label;
  x->role = r;
  x->n = n;
  x->runner = run;
  x->is_qemu = run != RUN_HOST;
  x->min_ms = x->median_ms = x->mad_ms = x->cpu_ms = 1e30;
  add_label(label);
  return x;
}

static void set_argv(row *r, char **av, int n) {
  for (int i = 0; i < n && i < MAX_ARGV - 1; ++i) r->argv[i] = av[i];
  r->argv[n < MAX_ARGV ? n : MAX_ARGV - 1] = NULL;
}

static int split_into(const char *s, char *out[], int max) {
  int n = 0;
  while (*s != '\0' && n < max) {
    while (*s == ' ' || *s == '\t') ++s;
    if (*s == '\0') break;
    const char *a = s;
    while (*s != '\0' && *s != ' ' && *s != '\t') ++s;
    out[n++] = xasprintf("%.*s", (int)(s - a), a);
  }
  return n;
}

/* --- statistics ---------------------------------------------------------- */

static int cmp_double(const void *a, const void *b) {
  double x = *(const double *)a, y = *(const double *)b;
  return x < y ? -1 : x > y;
}

static double median_of(const double *src, int n) {
  if (n <= 0) return 0.0;
  double tmp[MAX_REPS];
  for (int i = 0; i < n && i < MAX_REPS; ++i) tmp[i] = src[i];
  qsort(tmp, (size_t)n, sizeof tmp[0], cmp_double);
  return n % 2 ? tmp[n / 2] : (tmp[n / 2 - 1] + tmp[n / 2]) / 2.0;
}

static double mad_of(const double *src, int n) {
  if (n <= 0) return 0.0;
  double med = median_of(src, n), dev[MAX_REPS];
  for (int i = 0; i < n && i < MAX_REPS; ++i)
    dev[i] = src[i] > med ? src[i] - med : med - src[i];
  return median_of(dev, n);
}

static void summarize(row *r) {
  if (r->nreps == 0) return;
  r->min_ms = r->wall[0];
  for (int i = 1; i < r->nreps; ++i)
    if (r->wall[i] < r->min_ms) r->min_ms = r->wall[i];
  r->median_ms = median_of(r->wall, r->nreps);
  r->mad_ms = mad_of(r->wall, r->nreps);
  r->cpu_ms = median_of(r->cpu, r->nreps);
}

static void add_sample(row *r, int code, double wall, const struct rusage *ru) {
  r->exit_code = code;
  if (r->nreps >= MAX_REPS) return;
  r->wall[r->nreps] = wall;
  r->cpu[r->nreps] = (double)ru->ru_utime.tv_sec * 1000.0 +
                     (double)ru->ru_utime.tv_usec / 1000.0 +
                     (double)ru->ru_stime.tv_sec * 1000.0 +
                     (double)ru->ru_stime.tv_usec / 1000.0;
  ++r->nreps;
}

static void run_row(row *r, int reps) {
  if (!r->enabled) return;
  r->nreps = 0;
  for (int i = 0; i < reps && i < MAX_REPS; ++i) {
    struct rusage ru;
    double t0 = now_ms();
    int code = spawn_row(r, r->argv, &ru);
    add_sample(r, code, now_ms() - t0, &ru);
  }
  summarize(r);
}

typedef struct { double slope, intercept, r2; int k, valid; } fit_t;

static fit_t fit_values(const double *x, const double *y, int k) {
  fit_t f = { 0, 0, 0, k, 0 };
  if (k < 2) return f;
  double sx = 0, sy = 0, sxx = 0, sxy = 0;
  for (int i = 0; i < k; ++i) {
    sx += x[i]; sy += y[i]; sxx += x[i] * x[i]; sxy += x[i] * y[i];
  }
  double den = (double)k * sxx - sx * sx;
  if (den == 0.0) return f;
  f.slope = ((double)k * sxy - sx * sy) / den;
  f.intercept = (sy - f.slope * sx) / (double)k;
  double mean = sy / (double)k, sst = 0, sse = 0;
  for (int i = 0; i < k; ++i) {
    double pred = f.intercept + f.slope * x[i];
    sst += (y[i] - mean) * (y[i] - mean);
    sse += (y[i] - pred) * (y[i] - pred);
  }
  f.r2 = sst == 0.0 ? 1.0 : 1.0 - sse / sst;
  f.valid = 1;
  return f;
}

static int fit_label(const char *label, role r, int want_insn, fit_t *out) {
  double x[MAX_ROWS], y[MAX_ROWS];
  int k = 0;
  for (int i = 0; i < g_nrows; ++i) {
    row *v = &g_rows[i];
    if (v->role != r || strcmp(v->label, label) != 0 || !v->enabled ||
        v->nreps == 0)
      continue;
    if (want_insn && !v->insn_ok) continue;
    x[k] = (double)v->n;
    y[k] = want_insn ? (double)v->insn : v->median_ms;
    ++k;
  }
  if (k < 2) return 0;
  *out = fit_values(x, y, k);
  return out->valid;
}

static int fit_usable(const fit_t *f) {
  return f->valid && f->slope > 0.0 && f->r2 >= 0.5;
}

static row *row_at(const char *label, role r, long n) {
  for (int i = 0; i < g_nrows; ++i)
    if (g_rows[i].role == r && g_rows[i].n == n &&
        strcmp(g_rows[i].label, label) == 0)
      return &g_rows[i];
  return NULL;
}

/* --- workload rewriting -------------------------------------------------- */

static long read_bench_iters(const char *path) {
  FILE *f = fopen(path, "r");
  if (f == NULL) { perror(path); exit(2); }
  char line[2048];
  long n = -1;
  while (fgets(line, sizeof line, f) != NULL) {
    char *p = line;
    while (*p == ' ' || *p == '\t') ++p;
    if (strncmp(p, "let bench_iters", 15) == 0) {
      char *eq = strchr(p, '=');
      if (eq != NULL) n = strtol(eq + 1, NULL, 10);
      break;
    }
  }
  fclose(f);
  if (n <= 0) {
    fprintf(stderr, "bench: %s has no positive `let bench_iters = ...`\n", path);
    exit(2);
  }
  return n;
}

/* Top-level integers are nullary functions in rukaml, so the marker is
 * replaced by a literal everywhere outside its own binding. */
static int gen_source(const char *src, const char *dst, long n, int main_zero) {
  FILE *in = fopen(src, "r");
  if (in == NULL) { perror(src); return 0; }
  FILE *out = fopen(dst, "w");
  if (out == NULL) { fclose(in); perror(dst); return 0; }
  char line[4096];
  int iters = 0, main_done = 0;
  while (fgets(line, sizeof line, in) != NULL) {
    char *p = line;
    while (*p == ' ' || *p == '\t') ++p;
    if (!iters && strncmp(p, "let bench_iters", 15) == 0) { iters = 1; continue; }
    if (main_zero && !main_done && strncmp(p, "let main", 8) == 0) {
      fputs("let main = 0\n", out);
      main_done = 1;
      continue;
    }
    const char *q = line;
    for (const char *hit; (hit = strstr(q, "bench_iters")) != NULL;) {
      fwrite(q, 1, (size_t)(hit - q), out);
      fprintf(out, "%ld", n);
      q = hit + strlen("bench_iters");
    }
    fputs(q, out);
  }
  fclose(in);
  fclose(out);
  if (!iters) {
    fprintf(stderr, "bench: %s has no `let bench_iters`\n", src);
    return 0;
  }
  if (main_zero && !main_done) {
    fprintf(stderr, "bench: %s has no `let main`\n", src);
    return 0;
  }
  return 1;
}

static const char *workload_stem(const char *path) {
  const char *b = strrchr(path, '/');
  b = b != NULL ? b + 1 : path;
  size_t len = strlen(b);
  if (len > 3 && strcmp(b + len - 3, ".ml") == 0) len -= 3;
  char *s = xasprintf("%.*s", (int)len, b);
  for (char *p = s; *p != '\0'; ++p)
    if (!((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') ||
          (*p >= '0' && *p <= '9') || *p == '_' || *p == '-'))
      *p = '_';
  return s;
}

static int patch_rv64_exit(const char *path) {
  FILE *in = fopen(path, "r");
  if (in == NULL) return 0;
  char *tmp = xasprintf("%s.reexit", path);
  FILE *out = fopen(tmp, "w");
  if (out == NULL) { fclose(in); free(tmp); return 0; }
  char line[4096];
  int found = 0;
  while (fgets(line, sizeof line, in) != NULL) {
    if (!found && strstr(line, "Use 0 return code") != NULL) {
      found = 1;
      fputs("  # a0 keeps main's value, used as the exit status\n", out);
      continue;
    }
    fputs(line, out);
  }
  fclose(in);
  fclose(out);
  if (found && rename(tmp, path) == 0) { free(tmp); return 1; }
  remove(tmp);
  free(tmp);
  return 0;
}

/* The amd64 backend's _start does not export itself or load argc/argv, which
 * the runtime needs before user code runs. */
static int patch_amd64_start(const char *path) {
  FILE *in = fopen(path, "r");
  if (in == NULL) return 0;
  char *tmp = xasprintf("%s.start", path);
  FILE *out = fopen(tmp, "w");
  if (out == NULL) { fclose(in); free(tmp); return 0; }
  char line[4096];
  int in_start = 0, patched = 0;
  while (fgets(line, sizeof line, in) != NULL) {
    if (!in_start && strstr(line, "_start:") != NULL) {
      fputs("global _start\n", out);
      fputs(line, out);
      fputs("        mov     rdi, [rsp]      ; argc\n", out);
      fputs("        lea     rsi, [rsp+8]    ; argv\n", out);
      in_start = 1;
      patched = 1;
      continue;
    }
    if (in_start) {
      if (strstr(line, "call main") != NULL) {
        fputs(line, out);
        in_start = 0;
        continue;
      }
      if (strstr(line, "push") != NULL || strstr(line, "mov     rbp") != NULL ||
          strstr(line, "mov rbp") != NULL ||
          (strstr(line, "mov") != NULL && strstr(line, "rsp") != NULL))
        continue;
    }
    fputs(line, out);
  }
  fclose(in);
  fclose(out);
  if (patched && rename(tmp, path) == 0) { free(tmp); return 1; }
  remove(tmp);
  free(tmp);
  return 0;
}

/* --- AOT backends -------------------------------------------------------- */

typedef struct {
  const char *label, *id, *target, *cc, *ccflags, *as, *asflags, *ld, *ldflags;
  const char *runtime, *ext;
  runner run;
  int patch_exit, patch_start;
  char *runtime_obj;
} backend;

static backend g_backends[4];
static int g_nbackends;

static void add_backend(const char *label, const char *id, const char *target,
                        const char *cc, const char *ccflags, const char *as,
                        const char *asflags, const char *ld, const char *ldflags,
                        const char *runtime, const char *ext, runner run,
                        int patch_exit, int patch_start) {
  backend *b = &g_backends[g_nbackends++];
  *b = (backend){ label, id, target, cc, ccflags, as, asflags, ld, ldflags,
                  runtime, ext, run, patch_exit, patch_start, NULL };
}

static void init_backends(void) {
  add_backend("aot llvm -O3", "llvm", "llvm",
              env_or("CC_LLVM", "clang-19"),
              env_or("CFLAGS_LLVM", "-O2 -fPIC -w"),
              env_or("AS_LLVM", "clang-19"),
              env_or("AS_FLAGS_LLVM", "-O3 -Wno-override-module -x ir -c"),
              env_or("LD_LLVM", "clang-19"), env_or("LD_FLAGS_LLVM", "-O3"),
              env_or("RUKAML_RUNTIME_LLVM", "back/llvm/rukaml_stdlib.c"), "ll",
              RUN_HOST, 0, 0);
  add_backend("aot amd64", "amd64", "amd64", env_or("CC_AMD64", "cc"),
              env_or("CFLAGS_AMD64",
                     "-O2 -fPIC -w -D__amd64__=0 -include jit/bench/amd64_compat.h "
                     "-Wno-incompatible-pointer-types "
                     "-Wno-implicit-function-declaration"),
              env_or("AS_AMD64", "nasm"), env_or("AS_FLAGS_AMD64", "-f elf64"),
              env_or("LD_AMD64", "cc"),
              env_or("LD_FLAGS_AMD64", "-nostartfiles -no-pie"),
              env_or("RUKAML_RUNTIME_AMD64", "back/amd64/rukaml_stdlib.c"), "s",
              RUN_HOST, 0, 1);
  add_backend("aot rv64", "rv64", "rv64",
              env_or("CC_RV64", "riscv64-linux-gnu-gcc"),
              env_or("CFLAGS_RV64", "-O2 -fPIC -w -march=rv64gc"),
              env_or("AS_RV64", "riscv64-linux-gnu-gcc"),
              env_or("AS_FLAGS_RV64", "-x assembler -c -march=rv64gc"),
              env_or("LD_RV64", "riscv64-linux-gnu-gcc"),
              env_or("LD_FLAGS_RV64", "-no-pie"),
              env_or("RUKAML_RUNTIME_RV64", "back/rv64/rukaml_stdlib.c"), "s",
              RUN_QEMU64, 1, 0);
  add_backend("aot rv32", "rv32", "rv32",
              env_or("CC_RV32", "riscv64-linux-gnu-gcc"),
              env_or("CFLAGS_RV32", "-O2 -fPIC -w -march=rv32imac -mabi=ilp32"),
              env_or("AS_RV32", "riscv64-linux-gnu-gcc"),
              env_or("AS_FLAGS_RV32",
                     "-x assembler -c -march=rv32imac -mabi=ilp32"),
              env_or("LD_RV32", "riscv64-linux-gnu-gcc"),
              env_or("LD_FLAGS_RV32", "-no-pie"),
              env_or("RUKAML_RUNTIME_RV32", "back/rv64/rukaml_stdlib.c"), "s",
              RUN_QEMU32, 1, 0);
}

static const char *qemu_bin(runner r) {
  return r == RUN_QEMU64 ? env_or("QEMU_RV64", "qemu-riscv64")
                         : env_or("QEMU_RV32", "qemu-riscv32");
}

static const char *qemu_flags(runner r) {
  return r == RUN_QEMU64
             ? env_or("QEMU_RV64_FLAGS", "-L /usr/riscv64-linux-gnu -cpu rv64")
             : env_or("QEMU_RV32_FLAGS", "-L /usr/riscv32-linux-gnu -cpu rv32");
}

static int backend_ok(const backend *b) {
  if (!tool_ok(b->cc) || !tool_ok(b->as) || !tool_ok(b->ld) ||
      access(b->runtime, R_OK) != 0)
    return 0;
  return b->run == RUN_HOST || tool_ok(qemu_bin(b->run));
}

static int build_runner(runner run, const char *bin, char *av[], int max) {
  int k = 0;
  if (run == RUN_HOST) {
    av[k++] = (char *)bin;
  } else {
    av[k++] = (char *)qemu_bin(run);
    k += split_into(qemu_flags(run), &av[k], max - k - 1);
    av[k++] = (char *)bin;
  }
  av[k] = NULL;
  return k;
}

static int prepare_runtime(backend *b) {
  if (b->runtime_obj != NULL) return strcmp(b->runtime_obj, "-") != 0;
  char *obj = xasprintf("%s/stdlib.%s.o", g_dir, b->id);
  if (!run_cmd_quiet("%s %s -c %s -o %s", b->cc, b->ccflags, b->runtime, obj)) {
    b->runtime_obj = (char *)"-";
    return 0;
  }
  b->runtime_obj = obj;
  return 1;
}

static int prep_aot(backend *b, const char *src, long n, const char *label,
                    role r) {
  if (!backend_ok(b)) { note_skip(label, "toolchain not found"); return 0; }
  if (!prepare_runtime(b)) { note_skip(label, "runtime build failed"); return 0; }
  char *ir = xasprintf("%s/work.%s.%ld.%s", g_dir, b->id, n, b->ext);
  char *obj = xasprintf("%s/work.%s.%ld.o", g_dir, b->id, n);
  char *bin = xasprintf("%s/work.%s.%ld.exe", g_dir, b->id, n);
  if (!run_cmd("%s %s --target %s -o %s", g_driver, src, b->target, ir)) {
    note_skip(label, "driver failed");
    return 0;
  }
  int check = 1;
  if (b->patch_exit && !patch_rv64_exit(ir)) {
    fprintf(stderr, "bench: warning: cannot rewrite rv64 exit for %s\n", label);
    check = 0;
  }
  if (b->patch_start && !patch_amd64_start(ir)) {
    note_skip(label, "cannot patch amd64 startup");
    return 0;
  }
  if (!run_cmd("%s %s %s -o %s", b->as, b->asflags, ir, obj) ||
      !run_cmd("%s %s %s %s -o %s", b->ld, b->ldflags, b->runtime_obj, obj,
               bin)) {
    note_skip(label, "assemble/link failed");
    return 0;
  }
  row *x = new_row(label, r, n, b->run);
  build_runner(b->run, bin, x->argv, MAX_ARGV);
  x->enabled = 1;
  x->check_exit = check;
  return 1;
}

static int prep_jit(const char *src, long n, const char *label, role r) {
  row *x = new_row(label, r, n, RUN_HOST);
  char *av[] = { (char *)g_driver, (char *)src, "--target", "jit",
                 "--opt=online", NULL };
  set_argv(x, av, 5);
  x->enabled = 1;
  x->check_exit = 1;
  return 1;
}

/* --- qemu instruction counting ------------------------------------------ */

static int run_plugin(row *r, int *seq) {
  if (!r->is_qemu || g_plugin == NULL || r->argv[0] == NULL) return 0;
  char *av[MAX_ARGV];
  int k = 0;
  av[k++] = r->argv[0];
  av[k++] = "-plugin";
  av[k++] = (char *)g_plugin;
  for (int i = 1; r->argv[i] != NULL && k < MAX_ARGV - 1; ++i)
    av[k++] = r->argv[i];
  av[k] = NULL;
  char *out = xasprintf("%s/insn.%d.txt", g_dir, (*seq)++);
  unlink(out);
  setenv("QEMU_INSN_FILE", out, 1);
  int code = spawn_row(r, av, NULL);
  unsetenv("QEMU_INSN_FILE");
  if (code < 0) { free(out); return 0; }
  FILE *f = fopen(out, "r");
  if (f == NULL) { free(out); return 0; }
  unsigned long long count = 0;
  int ok = fscanf(f, "%llu", &count) == 1;
  fclose(f);
  remove(out);
  free(out);
  if (!ok) return 0;
  r->insn = count;
  r->insn_ok = 1;
  return 1;
}

static int count_deltas(const char *dir) {
  DIR *d = opendir(dir);
  if (d == NULL) return 0;
  int n = 0;
  for (struct dirent *e; (e = readdir(d)) != NULL;)
    if (strstr(e->d_name, "delta") != NULL) ++n;
  closedir(d);
  return n;
}

static int measure_tier_ups(const char *src) {
  char *dir = xasprintf("%s/online-dump", g_dir);
  mkdir(dir, 0755);
  char *av[] = { (char *)g_driver, (char *)src, "--target", "jit",
                 "--opt=online", "--opt-dump", dir, NULL };
  int code = spawn(av, NULL);
  int n = code < 0 ? -1 : count_deltas(dir);
  free(dir);
  return n;
}

/* --- comparison and reporting ------------------------------------------- */

typedef struct {
  int ok, has_fit, has_startup, has_probe;
  double jit_cold, aot_cold, cold_diff, cold_pct;
  double jit_fixed, aot_fixed, fixed_diff;
  double jit_steady, aot_steady, steady_diff, steady_pct;
  double jit_startup, aot_startup, startup_diff, jit_probe, compile_ms;
  int tier_ups;
} comparison;

static comparison get_comparison(void) {
  comparison c;
  memset(&c, 0, sizeof c);
  c.tier_ups = g_tier_ups;
  row *j = row_at("jit", ROLE_MAIN, g_base_n);
  row *a = row_at("aot llvm -O3", ROLE_MAIN, g_base_n);
  if (j == NULL || a == NULL || j->nreps == 0 || a->nreps == 0) return c;
  c.ok = 1;
  c.jit_cold = j->median_ms;
  c.aot_cold = a->median_ms;
  c.cold_diff = j->median_ms - a->median_ms;
  c.cold_pct = pct(c.cold_diff, a->median_ms);

  fit_t jf, af;
  if (fit_label("jit", ROLE_MAIN, 0, &jf) &&
      fit_label("aot llvm -O3", ROLE_MAIN, 0, &af) && fit_usable(&jf) &&
      fit_usable(&af)) {
    c.has_fit = 1;
    c.jit_fixed = jf.intercept;
    c.aot_fixed = af.intercept;
    c.fixed_diff = jf.intercept - af.intercept;
    c.jit_steady = jf.slope * 1e6;
    c.aot_steady = af.slope * 1e6;
    c.steady_diff = (jf.slope - af.slope) * 1e6;
    c.steady_pct = pct(jf.slope - af.slope, af.slope);
  }

  row *js = row_at("jit", ROLE_STARTUP, 0);
  row *as = row_at("aot llvm -O3", ROLE_STARTUP, 0);
  if (js != NULL && as != NULL && js->nreps > 0 && as->nreps > 0) {
    c.has_startup = 1;
    c.jit_startup = js->median_ms;
    c.aot_startup = as->median_ms;
    c.startup_diff = js->median_ms - as->median_ms;
  }
  row *jp = row_at("jit", ROLE_PROBE, g_base_n);
  if (jp != NULL && jp->nreps > 0) {
    c.has_probe = 1;
    c.jit_probe = jp->median_ms;
    if (c.has_startup) c.compile_ms = jp->median_ms - js->median_ms;
  }
  return c;
}

static void print_header(const char *workload) {
  printf("workload: %s\nbase N:   %ld\nsizes:    ", workload, g_base_n);
  for (int i = 0; i < g_nsizes; ++i) printf("%s%ld", i ? "," : "", g_sizes[i]);
  printf("\nreps:     %d\ndriver:   %s\nbaseline: aot llvm -O3\n", g_reps,
         g_driver);
  if (g_cpu >= 0) printf("cpu:      pinned to %d\n", g_cpu);
  printf("\n");
}

static void print_cold(void) {
  printf("cold (whole process, base N=%ld)\n", g_base_n);
  printf("%-20s %-11s %10s %10s %10s %10s %6s\n", "variant", "runner", "min(ms)",
         "median(ms)", "mad(ms)", "cpu(ms)", "exit");
  for (int li = 0; li < g_nlabels; ++li) {
    row *r = row_at(g_labels[li], ROLE_MAIN, g_base_n);
    if (r == NULL || r->nreps == 0) continue;
    printf("%-20s %-11s %10.1f %10.1f %10.1f %10.1f %6d\n", r->label,
           runner_name(r->runner), r->min_ms, r->median_ms, r->mad_ms, r->cpu_ms,
           r->exit_code);
  }
  printf("\n");
}

static void print_steady(void) {
  printf("steady state: T(N) = a + b*N (N = outer iterations)\n");
  printf("%-20s %12s %12s %8s %6s\n", "variant", "b(ns/iter)", "a(ms)", "R^2",
         "pts");
  int any_na = 0;
  for (int li = 0; li < g_nlabels; ++li) {
    fit_t f;
    if (!fit_label(g_labels[li], ROLE_MAIN, 0, &f)) continue;
    if (fit_usable(&f))
      printf("%-20s %12.2f %12.2f %8.3f %6d\n", g_labels[li], f.slope * 1e6,
             f.intercept, f.r2, f.k);
    else {
      any_na = 1;
      printf("%-20s %12s %12s %8.3f %6d\n", g_labels[li], "n/a", "n/a", f.r2,
             f.k);
    }
  }
  if (any_na)
    printf("  (n/a: slope <= 0 or R^2 < 0.5; usually the online JIT body is "
           "below timing noise)\n");
  printf("\n");
}

static void print_overhead(void) {
  comparison c = get_comparison();
  printf("JIT overhead vs aot llvm -O3 (positive = JIT slower)\n");
  if (!c.ok) {
    printf("jit or aot llvm -O3 is unavailable; overhead not computed\n\n");
    return;
  }
  printf("%-30s %12s %12s %12s %8s\n", "metric", "jit", "aot llvm -O3",
         "delta", "unit");
  printf("%-30s %12.1f %12.1f %+12.1f %8s\n", "cold, base N", c.jit_cold,
         c.aot_cold, c.cold_diff, "ms");
  if (c.has_fit)
    printf("%-30s %12.1f %12.1f %+12.1f %8s\n", "fixed (fit a)", c.jit_fixed,
           c.aot_fixed, c.fixed_diff, "ms");
  if (c.has_fit)
    printf("%-30s %12.2f %12.2f %+12.2f %8s\n", "steady (fit b)",
           c.jit_steady, c.aot_steady, c.steady_diff, "ns/iter");
  if (c.has_startup)
    printf("%-30s %12.1f %12.1f %+12.1f %8s\n", "startup (empty main)",
           c.jit_startup, c.aot_startup, c.startup_diff, "ms");
  if (c.has_probe && c.has_startup)
    printf("%-30s %12.1f %12.1f %+12.1f %8s\n",
           "JIT startup+compile (probe)", c.jit_probe, c.aot_startup,
           c.jit_probe - c.aot_startup, "ms");
  if (c.has_probe && c.has_startup)
    printf("%-30s %12.1f %12s %12s %8s\n", "JIT base compile",
           c.compile_ms, "-", "-", "ms");
  if (c.tier_ups >= 0)
    printf("%-30s %12d %12s %12s %8s\n", "online tier-ups", c.tier_ups, "-",
           "-", "count");
  printf("relative: cold %+.1f%%", c.cold_pct);
  if (c.has_fit) printf(", steady %+.1f%%", c.steady_pct);
  printf("\n\n");
}

static void print_points(void) {
  if (!g_verbose) return;
  printf("points (median per size)\n");
  printf("%-20s %8s %10s %10s %6s\n", "variant", "N", "median(ms)", "cpu(ms)",
         "exit");
  for (int li = 0; li < g_nlabels; ++li) {
    for (int i = 0; i < g_nrows; ++i) {
      row *r = &g_rows[i];
      if (r->role != ROLE_MAIN || strcmp(r->label, g_labels[li]) != 0 ||
          !r->enabled || r->nreps == 0)
        continue;
      printf("%-20s %8ld %10.2f %10.2f %6d\n", r->label, r->n, r->median_ms,
             r->cpu_ms, r->exit_code);
    }
  }
  printf("\n");
}

static void print_insn(void) {
  if (!g_qemu_insn) return;
  int any = 0;
  for (int li = 0; li < g_nlabels; ++li) {
    fit_t f;
    if (!fit_label(g_labels[li], ROLE_MAIN, 1, &f)) continue;
    if (!any) {
      printf("qemu guest instructions: I(N) = a + b*N\n");
      printf("%-20s %14s %16s %6s\n", "variant", "b(insn/iter)", "a(insn)",
             "pts");
      any = 1;
    }
    printf("%-20s %14.2f %16.2f %6d\n", g_labels[li], f.slope, f.intercept, f.k);
  }
  if (any) printf("\n");
}

static int check_results(void) {
  row *ref = row_at("aot llvm -O3", ROLE_MAIN, g_base_n);
  if (ref == NULL || !ref->check_exit || ref->nreps == 0) {
    ref = NULL;
    for (int i = 0; i < g_nrows; ++i) {
      row *r = &g_rows[i];
      if (r->role == ROLE_MAIN && r->n == g_base_n && r->enabled &&
          r->check_exit && r->nreps > 0) { ref = r; break; }
    }
  }
  if (ref == NULL) {
    printf("result check: no checked row at base N\n");
    return 0;
  }
  int bad = 0;
  for (int i = 0; i < g_nrows; ++i) {
    row *r = &g_rows[i];
    if (r->role != ROLE_MAIN || r->n != g_base_n || !r->enabled ||
        !r->check_exit || r->nreps == 0)
      continue;
    if (r->exit_code < 0) {
      fprintf(stderr, "bench: %s died from a signal\n", r->label);
      bad = 1;
    } else if (r->exit_code != ref->exit_code) {
      fprintf(stderr, "bench: exit mismatch: %s -> %d, expected %d (%s)\n",
              r->label, r->exit_code, ref->exit_code, ref->label);
      bad = 1;
    }
  }
  if (!bad)
    printf("result check: all rows agree (exit %d, reference %s)\n",
           ref->exit_code, ref->label);
  return bad;
}

/* --- JSON ---------------------------------------------------------------- */

static void jstr(FILE *f, const char *s) {
  fputc('"', f);
  for (; *s != '\0'; ++s) {
    if (*s == '"' || *s == '\\') fputc('\\', f);
    fputc(*s, f);
  }
  fputc('"', f);
}

static void jnum(FILE *f, int ok, double v) {
  if (ok) fprintf(f, "%.6f", v);
  else fputs("null", f);
}

static void write_json(const char *path, const char *workload) {
  FILE *f = fopen(path, "w");
  if (f == NULL) {
    fprintf(stderr, "bench: cannot write %s: %s\n", path, strerror(errno));
    return;
  }
  fprintf(f, "{\n  \"workload\": ");
  jstr(f, workload);
  fprintf(f, ",\n  \"base_n\": %ld,\n  \"sizes\": [", g_base_n);
  for (int i = 0; i < g_nsizes; ++i) fprintf(f, "%s%ld", i ? ", " : "", g_sizes[i]);
  fprintf(f, "],\n  \"reps\": %d,\n  \"baseline\": \"aot llvm -O3\",\n", g_reps);
  fputs("  \"cold\": [\n", f);
  int first = 1;
  for (int li = 0; li < g_nlabels; ++li) {
    row *r = row_at(g_labels[li], ROLE_MAIN, g_base_n);
    if (r == NULL || r->nreps == 0) continue;
    if (!first) fputs(",\n", f);
    first = 0;
    fprintf(f, "    {\"variant\": ");
    jstr(f, r->label);
    fprintf(f, ", \"runner\": ");
    jstr(f, runner_name(r->runner));
    fprintf(f, ", \"min_ms\": %.6f, \"median_ms\": %.6f, \"mad_ms\": %.6f, "
               "\"cpu_ms\": %.6f, \"exit\": %d}",
            r->min_ms, r->median_ms, r->mad_ms, r->cpu_ms, r->exit_code);
  }
  fputs("\n  ],\n  \"steady\": [\n", f);
  first = 1;
  for (int li = 0; li < g_nlabels; ++li) {
    fit_t fit;
    if (!fit_label(g_labels[li], ROLE_MAIN, 0, &fit)) continue;
    if (!first) fputs(",\n", f);
    first = 0;
    fprintf(f, "    {\"variant\": ");
    jstr(f, g_labels[li]);
    fprintf(f, ", \"usable\": %s, \"ns_per_iter\": %.6f, "
               "\"intercept_ms\": %.6f, \"r2\": %.6f, \"points\": %d}",
            fit_usable(&fit) ? "true" : "false", fit.slope * 1e6, fit.intercept,
            fit.r2, fit.k);
  }
  fputs("\n  ],\n  \"fixed\": [\n", f);
  first = 1;
  for (int i = 0; i < g_nrows; ++i) {
    row *r = &g_rows[i];
    if (r->role == ROLE_MAIN || !r->enabled || r->nreps == 0) continue;
    if (!first) fputs(",\n", f);
    first = 0;
    fprintf(f, "    {\"variant\": ");
    jstr(f, r->label);
    fprintf(f, ", \"role\": ");
    jstr(f, role_name(r->role));
    fprintf(f, ", \"median_ms\": %.6f}", r->median_ms);
  }
  fputs("\n  ],\n  \"insn\": [\n", f);
  first = 1;
  for (int li = 0; li < g_nlabels; ++li) {
    fit_t fit;
    if (!fit_label(g_labels[li], ROLE_MAIN, 1, &fit)) continue;
    if (!first) fputs(",\n", f);
    first = 0;
    fprintf(f, "    {\"variant\": ");
    jstr(f, g_labels[li]);
    fprintf(f, ", \"insn_per_iter\": %.6f, \"intercept\": %.6f, "
               "\"points\": %d}",
            fit.slope, fit.intercept, fit.k);
  }
  comparison c = get_comparison();
  fprintf(f, "\n  ],\n  \"comparison\": {\n    \"available\": %s,\n",
          c.ok ? "true" : "false");
  fputs("    \"cold_overhead_ms\": ", f); jnum(f, c.ok, c.cold_diff);
  fputs(",\n    \"cold_overhead_pct\": ", f); jnum(f, c.ok, c.cold_pct);
  fputs(",\n    \"fixed_overhead_ms\": ", f); jnum(f, c.has_fit, c.fixed_diff);
  fputs(",\n    \"steady_overhead_ns_per_iter\": ", f);
  jnum(f, c.has_fit, c.steady_diff);
  fputs(",\n    \"steady_overhead_pct\": ", f); jnum(f, c.has_fit, c.steady_pct);
  fputs(",\n    \"jit_startup_ms\": ", f); jnum(f, c.has_startup, c.jit_startup);
  fputs(",\n    \"aot_startup_ms\": ", f); jnum(f, c.has_startup, c.aot_startup);
  fputs(",\n    \"jit_compile_probe_ms\": ", f); jnum(f, c.has_probe, c.jit_probe);
  fputs(",\n    \"jit_prerun_overhead_ms\": ", f);
  jnum(f, c.has_probe && c.has_startup, c.jit_probe - c.aot_startup);
  fputs(",\n    \"jit_compile_ms\": ", f);
  jnum(f, c.has_probe && c.has_startup, c.compile_ms);
  fprintf(f, ",\n    \"online_tier_ups\": %d\n  }\n}\n", c.tier_ups);
  fclose(f);
}

/* --- main ---------------------------------------------------------------- */

static void raise_stack(void) {
  struct rlimit rl;
  if (getrlimit(RLIMIT_STACK, &rl) != 0) return;
  rlim_t want = rl.rlim_max == RLIM_INFINITY ? (rlim_t)2 * 1024 * 1024 * 1024
                                             : rl.rlim_max;
  struct rlimit nr = { want, rl.rlim_max };
  if (setrlimit(RLIMIT_STACK, &nr) != 0)
    fprintf(stderr, "bench: warning: cannot raise stack: %s\n", strerror(errno));
}

static void usage(const char *a0) {
  fprintf(stderr,
          "usage: %s [options] <workload.ml>\n"
          "  --reps N\n  --sizes N0,N1,...\n  --only PAT\n  --skip PAT\n"
          "  --exclude-backends PAT,...\n"
          "  --json FILE\n  --cpu N\n  --qemu-insn\n  --verbose\n",
          a0);
}

int main(int argc, char **argv) {
  const char *workload = NULL, *sizes_arg = NULL, *json = NULL;
  for (int i = 1; i < argc; ++i) {
    char *a = argv[i];
    if (strcmp(a, "--reps") == 0 && i + 1 < argc) g_reps = atoi(argv[++i]);
    else if (strcmp(a, "--sizes") == 0 && i + 1 < argc) sizes_arg = argv[++i];
    else if (strcmp(a, "--only") == 0 && i + 1 < argc) g_only = argv[++i];
    else if (strcmp(a, "--skip") == 0 && i + 1 < argc) g_skip = argv[++i];
    else if (strcmp(a, "--exclude-backends") == 0 && i + 1 < argc)
      add_backend_excludes(argv[++i]);
    else if (strcmp(a, "--json") == 0 && i + 1 < argc) json = argv[++i];
    else if (strcmp(a, "--cpu") == 0 && i + 1 < argc) g_cpu = atoi(argv[++i]);
    else if (strcmp(a, "--qemu-insn") == 0) g_qemu_insn = 1;
    else if (strcmp(a, "--verbose") == 0) g_verbose = 1;
    else if (strcmp(a, "--help") == 0 || strcmp(a, "-h") == 0) {
      usage(argv[0]); return 0;
    } else if (a[0] == '-') {
      fprintf(stderr, "bench: unknown option %s\n", a); usage(argv[0]); return 2;
    } else if (workload == NULL) workload = a;
    else { fprintf(stderr, "bench: unexpected argument %s\n", a); return 2; }
  }
  if (workload == NULL) { usage(argv[0]); return 2; }
  if (g_reps < 1) g_reps = 1;
  if (g_reps > MAX_REPS) g_reps = MAX_REPS;
  if (g_cpu < 0) {
    const char *e = getenv("RUKAML_BENCH_CPU");
    if (e != NULL && *e != '\0') g_cpu = atoi(e);
  }
  g_driver = env_or("RUKAML_DRIVER", "_build/default/driver/driver.exe");
  g_plugin = env_or("RUKAML_QEMU_PLUGIN", "jit/bench/qemu_plugin/libqemu_insn.so");
  g_base_n = read_bench_iters(workload);

  if (sizes_arg != NULL) {
    char *copy = xasprintf("%s", sizes_arg);
    char *p = copy;
    while (*p != '\0' && g_nsizes < MAX_SIZES) {
      char *end = NULL;
      long n = strtol(p, &end, 10);
      if (end == p) break;
      if (n > 0) g_sizes[g_nsizes++] = n;
      p = end;
      while (*p == ',' || *p == ' ') ++p;
    }
    free(copy);
  }
  if (g_nsizes == 0) {
    long half = g_base_n / 2;
    g_sizes[g_nsizes++] = half < 1 ? 1 : half;
    g_sizes[g_nsizes++] = g_base_n;
    g_sizes[g_nsizes++] = g_base_n * 2;
  } else {
    int has_base = 0;
    for (int i = 0; i < g_nsizes; ++i)
      if (g_sizes[i] == g_base_n) has_base = 1;
    if (!has_base && g_nsizes < MAX_SIZES) g_sizes[g_nsizes++] = g_base_n;
  }

  char dir[] = "/tmp/rukaml-bench-XXXXXX";
  if (mkdtemp(dir) == NULL) { perror("mkdtemp"); return 2; }
  g_dir = xasprintf("%s", dir);
  g_log = xasprintf("%s/prep.log", g_dir);
  raise_stack();
  init_backends();
  print_header(workload);

  const char *stem = workload_stem(workload);
  char *base_src = xasprintf("%s/base.%s.ml", g_dir, stem);
  if (!gen_source(workload, base_src, g_base_n, 0)) return 2;

  for (int si = 0; si < g_nsizes; ++si) {
    long n = g_sizes[si];
    char *src = xasprintf("%s/work.%s.%ld.ml", g_dir, stem, n);
    if (!gen_source(workload, src, n, 0)) return 2;
    if (selected("jit")) prep_jit(src, n, "jit", ROLE_MAIN);
    for (int bi = 0; bi < g_nbackends; ++bi)
      if (selected(g_backends[bi].label))
        prep_aot(&g_backends[bi], src, n, g_backends[bi].label, ROLE_MAIN);
  }

  char *empty_src = xasprintf("%s/empty.ml", g_dir);
  FILE *ef = fopen(empty_src, "w");
  if (ef == NULL) { perror(empty_src); return 2; }
  fputs("let main = 0\n", ef);
  fclose(ef);
  char *probe_src = xasprintf("%s/probe.%s.ml", g_dir, stem);
  if (!gen_source(workload, probe_src, g_base_n, 1)) return 2;

  /* Only the baseline AOT row needs an empty-main startup: it is the other
   * side of the JIT overhead comparison. */
  if (selected("jit")) {
    prep_jit(empty_src, 0, "jit", ROLE_STARTUP);
    prep_jit(probe_src, g_base_n, "jit", ROLE_PROBE);
  }
  if (selected("aot llvm -O3"))
    prep_aot(&g_backends[0], empty_src, 0, "aot llvm -O3", ROLE_STARTUP);

  for (int rep = 0; rep < g_reps; ++rep) {
    for (int i = 0; i < g_nrows; ++i) {
      row *r = &g_rows[i];
      if (r->role != ROLE_MAIN || !r->enabled) continue;
      struct rusage ru;
      double t0 = now_ms();
      int code = spawn_row(r, r->argv, &ru);
      add_sample(r, code, now_ms() - t0, &ru);
    }
  }
  for (int i = 0; i < g_nrows; ++i)
    if (g_rows[i].role == ROLE_MAIN) summarize(&g_rows[i]);
  for (int i = 0; i < g_nrows; ++i) {
    row *r = &g_rows[i];
    if (r->role != ROLE_MAIN && r->enabled) run_row(r, g_reps);
  }

  if (selected("jit")) g_tier_ups = measure_tier_ups(base_src);

  if (g_qemu_insn) {
    if (access(g_plugin, R_OK) != 0) {
      char *why = xasprintf("plugin not found: %s", g_plugin);
      note_skip("qemu insn", why);
      free(why);
    } else {
      int seq = 0;
      for (int i = 0; i < g_nrows; ++i) {
        row *r = &g_rows[i];
        if (r->role == ROLE_MAIN && r->enabled && r->is_qemu &&
            r->nreps > 0 && !run_plugin(r, &seq))
          note_skip(r->label, "qemu plugin run failed");
      }
    }
  }

  print_cold();
  print_steady();
  print_overhead();
  print_points();
  print_insn();
  for (int i = 0; i < g_nskips; ++i) printf("skipped: %s\n", g_skips[i]);
  if (g_nskips > 0) printf("\n");
  int bad = check_results();
  if (json != NULL) write_json(json, workload);
  return bad;
}
