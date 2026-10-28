/*
 * Benchmark harness: one rukaml workload against the online JIT, rukaml's
 * AOT LLVM backend at -O3 (the baseline) and the OCaml compilers.
 * See jit/bench/README.md.
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
#include <sys/time.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define MAX_ROWS 32
#define MAX_REPS 64
#define MAX_SIZES 8
#define MAX_ARGV 8
#define MAX_SKIPS 32

#define LABEL_JIT "jit"
#define LABEL_LLVM "aot llvm -O3"
#define LABEL_OCAMLC "ocamlc"
#define LABEL_OCAMLOPT "ocamlopt"

enum role { ROLE_MAIN, ROLE_STARTUP, ROLE_PROBE };

typedef struct {
  const char *label;
  enum role role;
  long n;
  char *argv[MAX_ARGV];
  int exit_code;
  int nreps;
  double wall[MAX_REPS], cpu[MAX_REPS];
  double min_ms, median_ms, mad_ms, cpu_ms;
} row;

typedef struct {
  double slope, intercept, r2;
  int k;
} fit;

static row g_rows[MAX_ROWS];
static int g_nrows;
static const char *g_skips[MAX_SKIPS];
static int g_nskips;

static const char *g_driver = "_build/default/driver/driver.exe";
static const char *g_only;
static const char *g_llvm_cc = "clang-19";
static const char *g_ocaml_src;
static const char *g_dir;
static const char *g_log;
static const char *g_llvm_rt;
static long g_base_n;
static long g_sizes[MAX_SIZES];
static int g_nsizes;
static int g_reps = 5;
static int g_cpu = -1;
static int g_verbose;
static int g_tier_ups = -1;

/* --- helpers ------------------------------------------------------------- */

static void die(const char *msg) {
  fprintf(stderr, "bench: %s\n", msg);
  exit(2);
}

static char *xvasprintf(const char *fmt, va_list ap) {
  char *s = NULL;
  if (vasprintf(&s, fmt, ap) < 0) die("out of memory");
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

static double now_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (double)ts.tv_sec * 1000.0 + (double)ts.tv_nsec / 1e6;
}

static double tv_ms(struct timeval tv) {
  return (double)tv.tv_sec * 1000.0 + (double)tv.tv_usec / 1000.0;
}

static double pct(double delta, double base) {
  return base == 0.0 ? 0.0 : 100.0 * delta / base;
}

/* Runs a build step; its output goes to the log, failures are reported. */
static int run_cmd(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  char *cmd = xvasprintf(fmt, ap);
  va_end(ap);
  char *full = xasprintf("%s >>%s 2>&1", cmd, g_log);
  int st = system(full);
  int ok = st != -1 && WIFEXITED(st) && WEXITSTATUS(st) == 0;
  if (!ok) fprintf(stderr, "bench: command failed: %s (log: %s)\n", cmd, g_log);
  free(cmd);
  free(full);
  return ok;
}

static int spawn(char *const av[], struct rusage *ru) {
  pid_t pid = fork();
  if (pid < 0) die("fork failed");
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
  if (wait4(pid, &status, 0, &r) < 0) die("wait4 failed");
  if (ru != NULL) *ru = r;
  return WIFSIGNALED(status) ? -1 : WEXITSTATUS(status);
}

static int selected(const char *label) {
  return g_only == NULL || strstr(label, g_only) != NULL;
}

static void skip(const char *label, const char *why) {
  if (g_nskips == MAX_SKIPS) return;
  g_skips[g_nskips++] = xasprintf("%s (%s)", label, why);
}

static row *add_row(const char *label, enum role r, long n, char *const argv[]) {
  if (g_nrows == MAX_ROWS) die("too many rows");
  row *x = &g_rows[g_nrows++];
  memset(x, 0, sizeof *x);
  x->label = label;
  x->role = r;
  x->n = n;
  for (int i = 0; argv[i] != NULL && i < MAX_ARGV - 1; ++i) x->argv[i] = argv[i];
  return x;
}

static row *find_row(const char *label, enum role r, long n) {
  for (int i = 0; i < g_nrows; ++i)
    if (g_rows[i].role == r && g_rows[i].n == n && strcmp(g_rows[i].label, label) == 0)
      return &g_rows[i];
  return NULL;
}

/* --- statistics ---------------------------------------------------------- */

static int cmp_double(const void *a, const void *b) {
  double x = *(const double *)a, y = *(const double *)b;
  return x < y ? -1 : x > y;
}

static double median_of(const double *src, int n) {
  if (n <= 0) return 0.0;
  double tmp[MAX_REPS];
  for (int i = 0; i < n; ++i) tmp[i] = src[i];
  qsort(tmp, (size_t)n, sizeof tmp[0], cmp_double);
  return n % 2 ? tmp[n / 2] : (tmp[n / 2 - 1] + tmp[n / 2]) / 2.0;
}

static double mad_of(const double *src, int n) {
  if (n <= 0) return 0.0;
  double med = median_of(src, n), dev[MAX_REPS];
  for (int i = 0; i < n; ++i) dev[i] = src[i] > med ? src[i] - med : med - src[i];
  return median_of(dev, n);
}

static void sample(row *r) {
  if (r->nreps == MAX_REPS) return;
  struct rusage ru;
  double t0 = now_ms();
  int code = spawn(r->argv, &ru);
  r->wall[r->nreps] = now_ms() - t0;
  r->cpu[r->nreps] = tv_ms(ru.ru_utime) + tv_ms(ru.ru_stime);
  r->exit_code = code;
  ++r->nreps;
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

/* Least-squares line through the main rows of [label], x = N, y = median ms. */
static fit fit_label(const char *label) {
  double x[MAX_SIZES], y[MAX_SIZES];
  int k = 0;
  for (int i = 0; i < g_nrows; ++i) {
    const row *r = &g_rows[i];
    if (r->role != ROLE_MAIN || strcmp(r->label, label) != 0 || r->nreps == 0) continue;
    if (k == MAX_SIZES) break;
    x[k] = (double)r->n;
    y[k] = r->median_ms;
    ++k;
  }
  fit f = { 0, 0, 0, k };
  if (k < 2) return f;
  double sx = 0, sy = 0, sxx = 0, sxy = 0;
  for (int i = 0; i < k; ++i) {
    sx += x[i];
    sy += y[i];
    sxx += x[i] * x[i];
    sxy += x[i] * y[i];
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
  return f;
}

static int fit_usable(const fit *f) { return f->k >= 2 && f->slope > 0.0 && f->r2 >= 0.5; }

/* --- workload sources --------------------------------------------------- */

static long read_bench_iters(const char *path) {
  FILE *f = fopen(path, "r");
  if (f == NULL) {
    perror(path);
    exit(2);
  }
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

/* Writes [src] with the `let bench_iters` binding removed and every other
 * occurrence replaced by [n].  Top-level integers are nullary functions in
 * rukaml, so a bare name would pass a closure.  With [main_zero] the `let main`
 * binding becomes `let main = 0` (the startup probe). */
static int write_source(const char *src, const char *dst, long n, int main_zero) {
  FILE *in = fopen(src, "r");
  if (in == NULL) {
    perror(src);
    return 0;
  }
  FILE *out = fopen(dst, "w");
  if (out == NULL) {
    fclose(in);
    perror(dst);
    return 0;
  }
  char line[4096];
  int has_iters = 0, has_main = 0;
  while (fgets(line, sizeof line, in) != NULL) {
    char *p = line;
    while (*p == ' ' || *p == '\t') ++p;
    if (strncmp(p, "let bench_iters", 15) == 0) {
      has_iters = 1;
      continue;
    }
    if (main_zero && strncmp(p, "let main", 8) == 0) {
      has_main = 1;
      fputs("let main = 0\n", out);
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
  if (!has_iters) fprintf(stderr, "bench: %s has no `let bench_iters`\n", src);
  if (main_zero && !has_main) fprintf(stderr, "bench: %s has no `let main`\n", src);
  return has_iters && (!main_zero || has_main);
}

/* workloads/<theme>/<name>.ml -> workloads/ocaml/<theme>/<name>.ocaml.ml */
static char *ocaml_twin_of(const char *workload) {
  const char *w = strstr(workload, "workloads/");
  if (w == NULL) return NULL;
  const char *rest = w + strlen("workloads/");
  size_t len = strlen(rest);
  if (len < 4 || strcmp(rest + len - 3, ".ml") != 0) return NULL;
  return xasprintf("%.*sworkloads/ocaml/%.*s.ocaml.ml", (int)(w - workload), workload,
                   (int)(len - 3), rest);
}

static const char *workload_stem(const char *path) {
  const char *b = strrchr(path, '/');
  b = b != NULL ? b + 1 : path;
  size_t len = strlen(b);
  if (len > 3 && strcmp(b + len - 3, ".ml") == 0) len -= 3;
  return xasprintf("%.*s", (int)len, b);
}

/* --- row builders -------------------------------------------------------- */

static void add_binary(const char *label, enum role r, long n, const char *bin) {
  char *argv[] = { (char *)bin, NULL };
  add_row(label, r, n, argv);
}

static void add_jit(const char *src, long n, enum role r) {
  if (!selected(LABEL_JIT)) return;
  char *argv[] = { (char *)g_driver, (char *)src, "--target", "jit", "--opt=online", NULL };
  add_row(LABEL_JIT, r, n, argv);
}

static void add_llvm(const char *src, long n, enum role r) {
  if (!selected(LABEL_LLVM)) return;
  if (g_llvm_rt == NULL) {
    skip(LABEL_LLVM, "runtime build failed");
    return;
  }
  char *ir = xasprintf("%s/llvm.%ld.ll", g_dir, n);
  char *obj = xasprintf("%s/llvm.%ld.o", g_dir, n);
  char *bin = xasprintf("%s/llvm.%ld.exe", g_dir, n);
  if (!run_cmd("%s %s --target llvm -o %s", g_driver, src, ir) ||
      !run_cmd("%s -O3 -Wno-override-module -x ir -c %s -o %s", g_llvm_cc, ir, obj) ||
      !run_cmd("%s -O3 %s %s -o %s", g_llvm_cc, g_llvm_rt, obj, bin))
    skip(LABEL_LLVM, "build failed");
  else
    add_binary(LABEL_LLVM, r, n, bin);
}

/* Builds the workload's OCaml twin with [compiler] (ocamlc or ocamlopt). */
static void add_ocaml(const char *label, const char *compiler, long n) {
  if (!selected(label)) return;
  if (g_ocaml_src == NULL || access(g_ocaml_src, R_OK) != 0) {
    skip(label, "no OCaml twin");
    return;
  }
  char *src = xasprintf("%s/%s.%ld.ml", g_dir, label, n);
  char *bin = xasprintf("%s/%s.%ld.exe", g_dir, label, n);
  if (!write_source(g_ocaml_src, src, n, 0) ||
      !run_cmd("%s -o %s %s", compiler, bin, src))
    skip(label, "build failed");
  else
    add_binary(label, ROLE_MAIN, n, bin);
}

/* --- reporting ----------------------------------------------------------- */

typedef struct {
  int has_cold, has_fit, has_startup, has_probe;
  double jit_cold, aot_cold, jit_fixed, aot_fixed, jit_steady, aot_steady;
  double jit_startup, aot_startup, jit_probe, compile_ms;
} overhead;

static overhead measure_overhead(void) {
  overhead o;
  memset(&o, 0, sizeof o);
  row *j = find_row(LABEL_JIT, ROLE_MAIN, g_base_n);
  row *a = find_row(LABEL_LLVM, ROLE_MAIN, g_base_n);
  if (j != NULL && a != NULL && j->nreps > 0 && a->nreps > 0) {
    o.has_cold = 1;
    o.jit_cold = j->median_ms;
    o.aot_cold = a->median_ms;
  }
  fit jf = fit_label(LABEL_JIT), af = fit_label(LABEL_LLVM);
  if (fit_usable(&jf) && fit_usable(&af)) {
    o.has_fit = 1;
    o.jit_fixed = jf.intercept;
    o.aot_fixed = af.intercept;
    o.jit_steady = jf.slope * 1e6;
    o.aot_steady = af.slope * 1e6;
  }
  row *js = find_row(LABEL_JIT, ROLE_STARTUP, 0);
  row *as = find_row(LABEL_LLVM, ROLE_STARTUP, 0);
  if (js != NULL && as != NULL && js->nreps > 0 && as->nreps > 0) {
    o.has_startup = 1;
    o.jit_startup = js->median_ms;
    o.aot_startup = as->median_ms;
  }
  row *jp = find_row(LABEL_JIT, ROLE_PROBE, g_base_n);
  if (jp != NULL && jp->nreps > 0) {
    o.has_probe = 1;
    o.jit_probe = jp->median_ms;
    if (o.has_startup) o.compile_ms = jp->median_ms - js->median_ms;
  }
  return o;
}

static void print_header(const char *workload) {
  printf("workload: %s\nbase N:   %ld\nsizes:    ", workload, g_base_n);
  for (int i = 0; i < g_nsizes; ++i) printf("%s%ld", i ? "," : "", g_sizes[i]);
  printf("\nreps:     %d\ndriver:   %s\nbaseline: %s\n", g_reps, g_driver, LABEL_LLVM);
  if (g_cpu >= 0) printf("cpu:      pinned to %d\n", g_cpu);
  printf("\n");
}

static void print_cold(void) {
  const row *base = find_row(LABEL_LLVM, ROLE_MAIN, g_base_n);
  printf("cold (whole process, base N=%ld)\n", g_base_n);
  printf("%-14s %10s %10s %10s %10s %6s %9s\n", "variant", "min(ms)", "median(ms)",
         "mad(ms)", "cpu(ms)", "exit", "vs llvm");
  for (int i = 0; i < g_nrows; ++i) {
    const row *r = &g_rows[i];
    if (r->role != ROLE_MAIN || r->n != g_base_n || r->nreps == 0) continue;
    printf("%-14s %10.1f %10.1f %10.1f %10.1f %6d ", r->label, r->min_ms, r->median_ms,
           r->mad_ms, r->cpu_ms, r->exit_code);
    if (base != NULL && base->nreps > 0)
      printf("%8.2fx\n", r->median_ms / base->median_ms);
    else
      printf("%9s\n", "-");
  }
  printf("\n");
}

static void print_steady(void) {
  printf("steady state: T(N) = a + b*N (N = outer iterations)\n");
  printf("%-14s %12s %12s %8s %6s\n", "variant", "b(ns/iter)", "a(ms)", "R^2", "pts");
  int any_na = 0;
  for (int i = 0; i < g_nrows; ++i) {
    const row *r = &g_rows[i];
    if (r->role != ROLE_MAIN || r->n != g_base_n) continue;
    fit f = fit_label(r->label);
    if (f.k < 2) continue;
    if (fit_usable(&f)) {
      printf("%-14s %12.2f %12.2f %8.3f %6d\n", r->label, f.slope * 1e6, f.intercept,
             f.r2, f.k);
    } else {
      any_na = 1;
      printf("%-14s %12s %12s %8.3f %6d\n", r->label, "n/a", "n/a", f.r2, f.k);
    }
  }
  if (any_na)
    printf("  (n/a: slope <= 0 or R^2 < 0.5; usually the online JIT body is below "
           "timing noise)\n");
  printf("\n");
}

static void print_overhead(void) {
  overhead o = measure_overhead();
  printf("JIT overhead vs %s (positive = JIT slower)\n", LABEL_LLVM);
  if (!o.has_cold) {
    printf("%s or %s is unavailable; overhead not computed\n\n", LABEL_JIT, LABEL_LLVM);
    return;
  }
  printf("%-30s %12s %12s %12s %8s\n", "metric", "jit", "aot llvm -O3", "delta", "unit");
  printf("%-30s %12.1f %12.1f %+12.1f %8s\n", "cold, base N", o.jit_cold, o.aot_cold,
         o.jit_cold - o.aot_cold, "ms");
  if (o.has_fit) {
    printf("%-30s %12.1f %12.1f %+12.1f %8s\n", "fixed (fit a)", o.jit_fixed,
           o.aot_fixed, o.jit_fixed - o.aot_fixed, "ms");
    printf("%-30s %12.2f %12.2f %+12.2f %8s\n", "steady (fit b)", o.jit_steady,
           o.aot_steady, o.jit_steady - o.aot_steady, "ns/iter");
  }
  if (o.has_startup)
    printf("%-30s %12.1f %12.1f %+12.1f %8s\n", "startup (empty main)", o.jit_startup,
           o.aot_startup, o.jit_startup - o.aot_startup, "ms");
  if (o.has_probe && o.has_startup) {
    printf("%-30s %12.1f %12.1f %+12.1f %8s\n", "JIT startup+compile (probe)",
           o.jit_probe, o.aot_startup, o.jit_probe - o.aot_startup, "ms");
    printf("%-30s %12.1f %12s %12s %8s\n", "JIT base compile", o.compile_ms, "-", "-",
           "ms");
  }
  if (g_tier_ups >= 0)
    printf("%-30s %12d %12s %12s %8s\n", "online tier-ups", g_tier_ups, "-", "-", "count");
  printf("relative: cold %+.1f%%", pct(o.jit_cold - o.aot_cold, o.aot_cold));
  if (o.has_fit) printf(", steady %+.1f%%", pct(o.jit_steady - o.aot_steady, o.aot_steady));
  printf("\n\n");
}

static void print_points(void) {
  if (!g_verbose) return;
  printf("points (median per size)\n");
  printf("%-14s %8s %10s %10s %6s\n", "variant", "N", "median(ms)", "cpu(ms)", "exit");
  for (int i = 0; i < g_nrows; ++i) {
    const row *r = &g_rows[i];
    if (r->role != ROLE_MAIN || r->nreps == 0) continue;
    printf("%-14s %8ld %10.2f %10.2f %6d\n", r->label, r->n, r->median_ms, r->cpu_ms,
           r->exit_code);
  }
  printf("\n");
}

static void print_skips(void) {
  for (int i = 0; i < g_nskips; ++i) printf("skipped: %s\n", g_skips[i]);
  if (g_nskips > 0) printf("\n");
}

/* Every main row at the base size must agree with the baseline's exit code. */
static int check_results(void) {
  const row *ref = find_row(LABEL_LLVM, ROLE_MAIN, g_base_n);
  if (ref == NULL || ref->nreps == 0) {
    printf("result check: skipped (no %s reference)\n", LABEL_LLVM);
    return 0;
  }
  int bad = 0;
  for (int i = 0; i < g_nrows; ++i) {
    const row *r = &g_rows[i];
    if (r->role != ROLE_MAIN || r->n != g_base_n || r->nreps == 0) continue;
    if (r->exit_code < 0) {
      fprintf(stderr, "bench: %s died from a signal\n", r->label);
      bad = 1;
    } else if (r->exit_code != ref->exit_code) {
      fprintf(stderr, "bench: exit mismatch: %s -> %d, expected %d (%s)\n", r->label,
              r->exit_code, ref->exit_code, ref->label);
      bad = 1;
    }
  }
  if (!bad)
    printf("result check: all rows agree (exit %d, reference %s)\n", ref->exit_code,
           ref->label);
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
  if (ok)
    fprintf(f, "%.6f", v);
  else
    fputs("null", f);
}

static void write_json(const char *path, const char *workload) {
  FILE *f = fopen(path, "w");
  if (f == NULL) {
    fprintf(stderr, "bench: cannot write %s: %s\n", path, strerror(errno));
    return;
  }
  fputs("{\n  \"workload\": ", f);
  jstr(f, workload);
  fprintf(f, ",\n  \"base_n\": %ld,\n  \"reps\": %d,\n  \"baseline\": \"%s\",\n", g_base_n,
          g_reps, LABEL_LLVM);
  fputs("  \"cold\": [\n", f);
  int first = 1;
  for (int i = 0; i < g_nrows; ++i) {
    const row *r = &g_rows[i];
    if (r->role != ROLE_MAIN || r->n != g_base_n || r->nreps == 0) continue;
    fputs(first ? "    " : ",\n    ", f);
    first = 0;
    fputs("{\"variant\": ", f);
    jstr(f, r->label);
    fprintf(f, ", \"min_ms\": %.6f, \"median_ms\": %.6f, \"mad_ms\": %.6f, "
               "\"cpu_ms\": %.6f, \"exit\": %d}",
            r->min_ms, r->median_ms, r->mad_ms, r->cpu_ms, r->exit_code);
  }
  fputs("\n  ],\n  \"steady\": [\n", f);
  first = 1;
  for (int i = 0; i < g_nrows; ++i) {
    const row *r = &g_rows[i];
    if (r->role != ROLE_MAIN || r->n != g_base_n) continue;
    fit fi = fit_label(r->label);
    if (fi.k < 2) continue;
    fputs(first ? "    " : ",\n    ", f);
    first = 0;
    fputs("{\"variant\": ", f);
    jstr(f, r->label);
    fprintf(f, ", \"usable\": %s, \"ns_per_iter\": %.6f, \"intercept_ms\": %.6f, "
               "\"r2\": %.6f, \"points\": %d}",
            fit_usable(&fi) ? "true" : "false", fi.slope * 1e6, fi.intercept, fi.r2, fi.k);
  }
  overhead o = measure_overhead();
  fputs("\n  ],\n  \"overhead\": {\n", f);
  fputs("    \"cold_ms\": ", f);
  jnum(f, o.has_cold, o.jit_cold - o.aot_cold);
  fputs(",\n    \"cold_pct\": ", f);
  jnum(f, o.has_cold, pct(o.jit_cold - o.aot_cold, o.aot_cold));
  fputs(",\n    \"steady_ns_per_iter\": ", f);
  jnum(f, o.has_fit, o.jit_steady - o.aot_steady);
  fputs(",\n    \"startup_ms\": ", f);
  jnum(f, o.has_startup, o.jit_startup - o.aot_startup);
  fputs(",\n    \"probe_ms\": ", f);
  jnum(f, o.has_probe, o.jit_probe);
  fputs(",\n    \"compile_ms\": ", f);
  jnum(f, o.has_probe && o.has_startup, o.compile_ms);
  fprintf(f, ",\n    \"online_tier_ups\": %d\n  }\n}\n", g_tier_ups);
  fclose(f);
}

/* --- main ---------------------------------------------------------------- */

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
  char *av[] = { (char *)g_driver, (char *)src, "--target", "jit", "--opt=online",
                 "--opt-dump", dir, NULL };
  int code = spawn(av, NULL);
  int n = code < 0 ? -1 : count_deltas(dir);
  free(dir);
  return n;
}

static void raise_stack(void) {
  struct rlimit rl;
  if (getrlimit(RLIMIT_STACK, &rl) != 0) return;
  rlim_t want = rl.rlim_max == RLIM_INFINITY ? (rlim_t)2 * 1024 * 1024 * 1024 : rl.rlim_max;
  struct rlimit nr = { want, rl.rlim_max };
  if (setrlimit(RLIMIT_STACK, &nr) != 0)
    fprintf(stderr, "bench: warning: cannot raise stack: %s\n", strerror(errno));
}

static void build_llvm_runtime(void) {
  g_llvm_cc = env_or("CC_LLVM", g_llvm_cc);
  char *obj = xasprintf("%s/rukaml_stdlib.o", g_dir);
  if (run_cmd("%s -O2 -fPIC -w -c back/llvm/rukaml_stdlib.c -o %s", g_llvm_cc, obj))
    g_llvm_rt = obj;
}

static void usage(const char *a0) {
  fprintf(stderr,
          "usage: %s [options] <workload.ml>\n"
          "  --reps N         timed repetitions per row (default 5, max %d)\n"
          "  --sizes N0,N1,.. sizes for the steady fit (default base/2,base,2*base)\n"
          "  --only PAT       keep rows whose label contains PAT\n"
          "  --cpu N          pin children to CPU N (also RUKAML_BENCH_CPU=N)\n"
          "  --json FILE      write results as JSON\n"
          "  --verbose        print the median of every size\n",
          a0, MAX_REPS);
}

static void parse_sizes(const char *arg) {
  char *p = xasprintf("%s", arg);
  while (*p != '\0' && g_nsizes < MAX_SIZES) {
    char *end = NULL;
    long n = strtol(p, &end, 10);
    if (end == p) break;
    if (n > 0) g_sizes[g_nsizes++] = n;
    p = end;
    while (*p == ',' || *p == ' ') ++p;
  }
}

static void choose_sizes(void) {
  if (g_nsizes == 0) {
    long half = g_base_n / 2;
    g_sizes[g_nsizes++] = half < 1 ? 1 : half;
    g_sizes[g_nsizes++] = g_base_n;
    g_sizes[g_nsizes++] = g_base_n * 2;
    return;
  }
  for (int i = 0; i < g_nsizes; ++i)
    if (g_sizes[i] == g_base_n) return;
  if (g_nsizes < MAX_SIZES) g_sizes[g_nsizes++] = g_base_n;
}

int main(int argc, char **argv) {
  const char *workload = NULL, *sizes_arg = NULL, *json = NULL;
  for (int i = 1; i < argc; ++i) {
    const char *a = argv[i];
    int has_next = i + 1 < argc;
    if (strcmp(a, "--reps") == 0 && has_next) g_reps = atoi(argv[++i]);
    else if (strcmp(a, "--sizes") == 0 && has_next) sizes_arg = argv[++i];
    else if (strcmp(a, "--only") == 0 && has_next) g_only = argv[++i];
    else if (strcmp(a, "--cpu") == 0 && has_next) g_cpu = atoi(argv[++i]);
    else if (strcmp(a, "--json") == 0 && has_next) json = argv[++i];
    else if (strcmp(a, "--verbose") == 0) g_verbose = 1;
    else if (strcmp(a, "--help") == 0 || strcmp(a, "-h") == 0) {
      usage(argv[0]);
      return 0;
    } else if (a[0] == '-') {
      fprintf(stderr, "bench: unknown option %s\n", a);
      usage(argv[0]);
      return 2;
    } else if (workload == NULL) {
      workload = a;
    } else {
      fprintf(stderr, "bench: unexpected argument %s\n", a);
      return 2;
    }
  }
  if (workload == NULL) {
    usage(argv[0]);
    return 2;
  }
  if (g_reps < 1) g_reps = 1;
  if (g_reps > MAX_REPS) g_reps = MAX_REPS;
  if (g_cpu < 0) {
    const char *e = getenv("RUKAML_BENCH_CPU");
    if (e != NULL && *e != '\0') g_cpu = atoi(e);
  }
  g_driver = env_or("RUKAML_DRIVER", g_driver);
  g_base_n = read_bench_iters(workload);
  if (sizes_arg != NULL) parse_sizes(sizes_arg);
  choose_sizes();

  char dir[] = "/tmp/rukaml-bench-XXXXXX";
  if (mkdtemp(dir) == NULL) die("cannot create a scratch directory");
  g_dir = xasprintf("%s", dir);
  g_log = xasprintf("%s/build.log", g_dir);
  g_ocaml_src = ocaml_twin_of(workload);
  raise_stack();
  print_header(workload);

  const char *stem = workload_stem(workload);
  build_llvm_runtime();

  char *base_src = xasprintf("%s/%s.base.ml", g_dir, stem);
  if (!write_source(workload, base_src, g_base_n, 0)) return 2;

  for (int si = 0; si < g_nsizes; ++si) {
    long n = g_sizes[si];
    char *src = xasprintf("%s/%s.%ld.ml", g_dir, stem, n);
    if (!write_source(workload, src, n, 0)) return 2;
    add_jit(src, n, ROLE_MAIN);
    add_llvm(src, n, ROLE_MAIN);
    add_ocaml(LABEL_OCAMLC, "ocamlc", n);
    add_ocaml(LABEL_OCAMLOPT, "ocamlopt", n);
  }

  /* Startup: an empty program measures process + runtime setup; the probe
   * compiles the workload without running it. */
  char *empty_src = xasprintf("%s/empty.ml", g_dir);
  FILE *ef = fopen(empty_src, "w");
  if (ef == NULL) die("cannot write the startup program");
  fputs("let main = 0\n", ef);
  fclose(ef);
  char *probe_src = xasprintf("%s/%s.probe.ml", g_dir, stem);
  if (!write_source(workload, probe_src, g_base_n, 1)) return 2;
  add_jit(empty_src, 0, ROLE_STARTUP);
  add_llvm(empty_src, 0, ROLE_STARTUP);
  add_jit(probe_src, g_base_n, ROLE_PROBE);

  for (int rep = 0; rep < g_reps; ++rep)
    for (int i = 0; i < g_nrows; ++i) sample(&g_rows[i]);
  for (int i = 0; i < g_nrows; ++i) summarize(&g_rows[i]);

  if (selected(LABEL_JIT)) g_tier_ups = measure_tier_ups(base_src);

  print_cold();
  print_steady();
  print_overhead();
  print_points();
  print_skips();
  int bad = check_results();
  if (json != NULL) write_json(json, workload);
  return bad;
}
