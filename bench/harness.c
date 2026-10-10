/* Benchmark harness: runs one rukaml workload through every available
 * execution path and reports wall-clock time —
 *
 *   jit             driver --target jit
 *   jit+specialize  driver --target jit --opt=specialize (profile gathered
 *                   once, before timing)
 *   aot llvm        driver --target llvm  + clang
 *   aot amd64       driver --target amd64 + nasm + gcc
 *   aot rv64        driver --target rv64  + riscv64 gcc, run under qemu
 *   ocamlopt        ocamlopt on the companion <workload>.ocaml.ml
 *   ocamlc          ocamlc   on the companion <workload>.ocaml.ml
 *
 * Nothing else runs in the timed region: every binary/profile is produced up
 * front, so a measurement is the child process's whole lifetime (exec +
 * compile + run for the JIT, exec + run for the native backends).
 *
 * Build and run from the repo root (see `make bench`):
 *
 *   cc -O2 -std=c11 -Wall -Wextra -o bench/harness bench/harness.c
 *   ./bench/harness bench/workloads/curried.ml [reps]
 *
 * A backend whose toolchain is not installed is skipped with a note rather
 * than failing the run. Overrides via the environment:
 *
 *   RUKAML_DRIVER, CLANG / CC_LLVM, AS_LLVM, LD_LLVM, RUN_LLVM, ...
 *   CC_AMD64, AS_AMD64, AS_FLAGS_AMD64, LD_AMD64, RUN_AMD64, ...
 *   CC_RV64,  AS_RV64,  AS_FLAGS_RV64,  LD_RV64,  RUN_RV64,  ...
 *   OCAMLOPT, OCAMLC, RUKAML_RUNTIME_{LLVM,AMD64,RV64} */

#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#define MAX_VARIANTS 16
#define ARENA_SIZE (1 << 20)

/* --- persistent strings -------------------------------------------------- */

static char g_arena[ARENA_SIZE];
static size_t g_arena_used = 0;

static char *afmt(const char *fmt, ...) {
  char *p = g_arena + g_arena_used;
  size_t left = ARENA_SIZE - g_arena_used;
  va_list ap;
  va_start(ap, fmt);
  int n = vsnprintf(p, left, fmt, ap);
  va_end(ap);
  if (n < 0 || (size_t)n >= left) {
    fprintf(stderr, "bench: string arena exhausted\n");
    exit(2);
  }
  g_arena_used += (size_t)n + 1;
  return p;
}

static const char *env_or(const char *name, const char *def) {
  const char *v = getenv(name);
  return (v != NULL && *v != '\0') ? v : def;
}

/* --- time and processes -------------------------------------------------- */

static double now_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (double)ts.tv_sec * 1000.0 + (double)ts.tv_nsec / 1e6;
}

/* Runs argv to completion; returns the exit status, or -1 when the child
 * was killed by a signal (a stack overflow looks like SIGSEGV). */
static int spawn(char *const av[]) {
  pid_t pid = fork();
  if (pid < 0) {
    perror("fork");
    exit(2);
  }
  if (pid == 0) {
    execvp(av[0], av);
    fprintf(stderr, "bench: cannot run %s: %s\n", av[0], strerror(errno));
    _exit(127);
  }
  int status = 0;
  if (waitpid(pid, &status, 0) < 0) {
    perror("waitpid");
    exit(2);
  }
  return WIFSIGNALED(status) ? -1 : WEXITSTATUS(status);
}

static int have_tool(const char *name) {
  if (strchr(name, '/') != NULL) {
    return access(name, X_OK) == 0;
  }
  const char *path = getenv("PATH");
  if (path == NULL) {
    return 0;
  }
  char buf[PATH_MAX];
  for (const char *p = path; *p != '\0';) {
    const char *e = strchr(p, ':');
    size_t len = (e != NULL) ? (size_t)(e - p) : strlen(p);
    if (len == 0) {
      snprintf(buf, sizeof buf, "./%s", name);
    } else {
      snprintf(buf, sizeof buf, "%.*s/%s", (int)len, p, name);
    }
    if (access(buf, X_OK) == 0) {
      return 1;
    }
    if (e == NULL) {
      break;
    }
    p = e + 1;
  }
  return 0;
}

/* --- shell commands for preparation (untimed) ---------------------------- */

static const char *g_log;
static const char *g_driver;

static int system_ok(const char *fmt, ...) {
  char cmd[8192];
  va_list ap;
  va_start(ap, fmt);
  int n = vsnprintf(cmd, sizeof cmd, fmt, ap);
  va_end(ap);
  if (n < 0 || (size_t)n >= sizeof cmd) {
    fprintf(stderr, "bench: command too long\n");
    exit(2);
  }
  size_t len = strlen(cmd);
  snprintf(cmd + len, sizeof cmd - len, " >>%s 2>&1", g_log);
  int st = system(cmd);
  return st != -1 && WIFEXITED(st) && WEXITSTATUS(st) == 0;
}

static void dump_log(void) {
  FILE *f = fopen(g_log, "r");
  if (f == NULL) {
    return;
  }
  char line[512];
  for (int i = 0; i < 40 && fgets(line, sizeof line, f) != NULL; ++i) {
    fputs(line, stderr);
  }
  fclose(f);
}

/* --- variants ------------------------------------------------------------ */

typedef struct {
  const char *label;
  char *av[16];
  int enabled;
  int exit_code;
  int check_exit; /* include in the result-equivalence check */
  double min_ms;
  double mean_ms;
} variant;

static variant g_vars[MAX_VARIANTS];
static int g_ng = 0;
static const char *g_skips[MAX_VARIANTS];
static int g_nskips = 0;

static variant *new_variant(const char *label) {
  variant *v = &g_vars[g_ng++];
  *v = (variant){ .label = label, .min_ms = 1e30, .check_exit = 1 };
  return v;
}

static void note_skip(const char *label, const char *why) {
  if (g_nskips < MAX_VARIANTS) {
    g_skips[g_nskips++] = afmt("%s (%s)", label, why);
  }
}

static void set_av(variant *v, char **av, int n) {
  for (int i = 0; i < n; ++i) {
    v->av[i] = av[i];
  }
  v->av[n] = NULL;
  v->enabled = 1;
}

/* Splits a space-separated command prefix (may be empty) into argv. */
static int split_into(const char *s, char *out[], int max) {
  int n = 0;
  while (*s != '\0' && n < max) {
    while (*s == ' ') {
      ++s;
    }
    if (*s == '\0') {
      break;
    }
    const char *start = s;
    while (*s != '\0' && *s != ' ') {
      ++s;
    }
    out[n++] = afmt("%.*s", (int)(s - start), start);
  }
  return n;
}

static void run_variant(variant *v, int reps) {
  for (int i = 0; i < reps; ++i) {
    double t0 = now_ms();
    v->exit_code = spawn(v->av);
    double dt = now_ms() - t0;
    if (dt < v->min_ms) {
      v->min_ms = dt;
    }
    v->mean_ms += dt;
  }
  v->mean_ms /= reps;
}

static void print_row(const variant *v) {
  char code[16];
  if (v->exit_code < 0) {
    snprintf(code, sizeof code, "SIG");
  } else {
    snprintf(code, sizeof code, "%d", v->exit_code);
  }
  printf("%-24s %10.1f %10.1f %8s\n", v->label, v->min_ms, v->mean_ms, code);
}

/* --- AOT backends -------------------------------------------------------- */

typedef struct {
  const char *pretty; /* table label */
  const char *tgt;    /* rukaml --target value */
  const char *cc;     /* compiler for the runtime */
  const char *ccflags;
  const char *as;     /* assembler */
  const char *asflags;
  const char *ld;     /* linker */
  const char *ldflags;
  const char *run;    /* run prefix, e.g. "qemu-riscv64 -L ..." */
  const char *ext;    /* compiled source extension: "s" or "ll" */
  const char *runtime;
  int patch_exit;     /* rewrite the exit sequence so main's value is observable */
} backend;

/* The rv64 backend is freestanding: its generated `main` ends with a raw
 * `addi a0, x0, 0; addi a7, x0, 93; ecall`, so it always exits 0 and its result
 * is only observable through `print`. To let the rv64 row take part in the
 * exit-code equivalence check, rewrite that single instruction so a0 — which
 * already holds main's value — becomes the exit status. The computation is
 * untouched (the instruction runs once, after it). Returns 1 on success and 0
 * if the expected sequence is not there, in which case the row is timed but
 * left out of the check. */
static int patch_rv64_exit(const char *path) {
  FILE *in = fopen(path, "r");
  if (in == NULL) {
    return 0;
  }
  char tmp[PATH_MAX];
  snprintf(tmp, sizeof tmp, "%s.reexit", path);
  FILE *out = fopen(tmp, "w");
  if (out == NULL) {
    fclose(in);
    return 0;
  }
  char line[2048];
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
  if (found && rename(tmp, path) == 0) {
    return 1;
  }
  remove(tmp);
  return 0;
}

static void prep_aot(const backend *b, const char *workload, const char *dir) {
  variant *v = new_variant(b->pretty);
  if (!have_tool(b->cc) || !have_tool(b->as) || !have_tool(b->ld)) {
    note_skip(b->pretty, "toolchain not found");
    return;
  }
  char *src = afmt("%s/work.%s.%s", dir, b->tgt, b->ext);
  char *obj = afmt("%s/work.%s.o", dir, b->tgt);
  char *slib = afmt("%s/stdlib.%s.o", dir, b->tgt);
  char *bin = afmt("%s/work.%s.exe", dir, b->tgt);

  const char *step = "the runtime";
  int ok = system_ok("%s %s -c %s -o %s", b->cc, b->ccflags, b->runtime, slib);
  if (ok) {
    step = "the workload IR/assembly";
    ok = system_ok("%s %s --target %s -o %s", g_driver, workload, b->tgt, src);
  }
  if (ok && b->patch_exit && !patch_rv64_exit(src)) {
    fprintf(stderr,
            "bench: warning: could not rewrite the rv64 exit sequence; "
            "the rv64 row is timed but not result-checked\n");
    v->check_exit = 0;
  }
  if (ok) {
    step = "the assembly";
    ok = system_ok("%s %s %s -o %s", b->as, b->asflags, src, obj);
  }
  if (ok) {
    step = "the link";
    ok = system_ok("%s %s %s %s -o %s", b->ld, b->ldflags, slib, obj, bin);
  }
  if (!ok) {
    fprintf(stderr, "bench: building %s for %s failed:\n", step, b->pretty);
    dump_log();
    exit(2);
  }
  int n = split_into(b->run, v->av, 8);
  v->av[n] = bin;
  v->av[n + 1] = NULL;
  v->enabled = 1;
}

/* --- OCaml comparison ---------------------------------------------------- */

static void prep_ocaml(const char *label, const char *stem, const char *compiler,
                       const char *ocaml_src, const char *dir) {
  variant *v = new_variant(label);
  if (!have_tool(compiler)) {
    note_skip(label, "not found");
    return;
  }
  /* OCaml drops <src>.cmi/<src>.cmx/<src>.o next to the *source*, so compile a
   * copy inside the run directory instead of littering the workloads tree. */
  char *src = afmt("%s/%s.ml", dir, stem);
  char *bin = afmt("%s/%s.bin", dir, stem);
  if (!system_ok("cp %s %s", ocaml_src, src) ||
      !system_ok("%s %s -o %s", compiler, src, bin)) {
    fprintf(stderr, "bench: building %s with %s failed:\n", ocaml_src, compiler);
    dump_log();
    exit(2);
  }
  char *av[2] = { bin, NULL };
  set_av(v, av, 1);
}

static char *ocaml_companion(const char *workload) {
  size_t len = strlen(workload);
  if (len > 3 && strcmp(workload + len - 3, ".ml") == 0) {
    return afmt("%.*s.ocaml.ml", (int)(len - 3), workload);
  }
  return afmt("%s.ocaml.ml", workload);
}

/* --- stack --------------------------------------------------------------- */

/* rukaml emits no `tail` markers, so neither the JIT's codegen nor clang's
 * -O2 turns a workload's self-recursion into a loop and a few million
 * iterations need hundreds of MiB of stack. Raise the soft limit and let
 * children inherit it.
 *
 * An unlimited hard limit is deliberately *not* propagated: qemu-user sizes
 * the guest stack from RLIMIT_STACK and, given RLIM_INFINITY, falls back to a
 * small default (a deep rv64 run then SIGSEGVs even though the host stack is
 * unbounded). A large finite value is honoured by both the kernel and qemu. */
#define STACK_WANT ((rlim_t)2 * 1024 * 1024 * 1024) /* 2 GiB */
static void raise_stack(void) {
  struct rlimit rl;
  if (getrlimit(RLIMIT_STACK, &rl) != 0) {
    perror("getrlimit");
    return;
  }
  rlim_t want =
      (rl.rlim_max == RLIM_INFINITY) ? STACK_WANT : rl.rlim_max;
  struct rlimit nr = { want, rl.rlim_max };
  if (setrlimit(RLIMIT_STACK, &nr) != 0) {
    fprintf(stderr, "bench: warning: cannot raise the stack limit: %s\n",
            strerror(errno));
  }
}

/* --- main ---------------------------------------------------------------- */

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "usage: %s <workload.ml> [reps]\n", argv[0]);
    return 2;
  }
  const char *workload = argv[1];
  int reps = argc > 2 ? atoi(argv[2]) : 5;
  if (reps < 1) {
    reps = 1;
  }
  const char *driver = env_or("RUKAML_DRIVER", "_build/default/driver/driver.exe");
  g_driver = driver;

  raise_stack();

  char dir[] = "/tmp/rukaml-bench-XXXXXX";
  if (mkdtemp(dir) == NULL) {
    perror("mkdtemp");
    return 2;
  }
  g_log = afmt("%s/prep.log", dir);
  FILE *lf = fopen(g_log, "w");
  if (lf != NULL) {
    fclose(lf);
  }

  printf("workload: %s   reps: %d\n", workload, reps);
  printf("driver:   %s\n\n", driver);

  /* jit */
  {
    variant *v = new_variant("jit");
    char *av[] = { (char *)driver, (char *)workload, "--target", "jit", NULL };
    set_av(v, av, 4);
  }

  /* jit+specialize: gather a profile once, untimed */
  {
    char *prof = afmt("%s/work.prof", dir);
    char *prof_run[] = { (char *)driver, (char *)workload, "--target", "jit",
                         "--opt=profile", "--profile-file", prof, NULL };
    if (spawn(prof_run) < 0 || access(prof, R_OK) != 0) {
      fprintf(stderr, "bench: the profile run produced no profile\n");
      return 2;
    }
    variant *v = new_variant("jit+specialize");
    char *av[] = { (char *)driver, (char *)workload, "--target", "jit",
                   "--opt=specialize", "--profile-file", prof, NULL };
    set_av(v, av, 7);
  }

  /* AOT backends */
  backend be_llvm = { "aot llvm (clang)",
                      "llvm",
                      env_or("CC_LLVM", "clang-19"),
                      env_or("CFLAGS_LLVM", "-O2 -fPIC -w"),
                      env_or("AS_LLVM", "clang-19"),
                      env_or("AS_FLAGS_LLVM", "-O2 -Wno-override-module -x ir -c"),
                      env_or("LD_LLVM", "clang-19"),
                      env_or("LD_FLAGS_LLVM", "-O2"),
                      env_or("RUN_LLVM", ""),
                      "ll",
                      env_or("RUKAML_RUNTIME_LLVM", "back/llvm/rukaml_stdlib.c"),
                      0 };
  backend be_amd64 = { "aot amd64 (nasm+gcc)",
                       "amd64",
                       env_or("CC_AMD64", "cc"),
                       env_or("CFLAGS_AMD64", "-O2 -fPIC -w"),
                       env_or("AS_AMD64", "nasm"),
                       env_or("AS_FLAGS_AMD64", "-f elf64"),
                       env_or("LD_AMD64", "cc"),
                       env_or("LD_FLAGS_AMD64", ""),
                       env_or("RUN_AMD64", ""),
                       "s",
                       env_or("RUKAML_RUNTIME_AMD64", "back/amd64/rukaml_stdlib.c"),
                       0 };
  backend be_rv64 = { "aot rv64 (qemu)",
                      "rv64",
                      env_or("CC_RV64", "riscv64-linux-gnu-gcc"),
                      env_or("CFLAGS_RV64", "-O2 -fPIC -w"),
                      env_or("AS_RV64", "riscv64-linux-gnu-gcc"),
                      env_or("AS_FLAGS_RV64", "-x assembler -c"),
                      env_or("LD_RV64", "riscv64-linux-gnu-gcc"),
                      env_or("LD_FLAGS_RV64", ""),
                      env_or("RUN_RV64", "qemu-riscv64 -L /usr/riscv64-linux-gnu"),
                      "s",
                      env_or("RUKAML_RUNTIME_RV64", "back/rv64/rukaml_stdlib.c"),
                      1 };
  prep_aot(&be_llvm, workload, dir);
  prep_aot(&be_amd64, workload, dir);
  prep_aot(&be_rv64, workload, dir);

  /* OCaml comparison, if a companion source exists */
  char *ocaml_src = ocaml_companion(workload);
  if (access(ocaml_src, R_OK) == 0) {
    prep_ocaml("ocamlopt", "work_ocamlopt", env_or("OCAMLOPT", "ocamlopt"),
               ocaml_src, dir);
    prep_ocaml("ocamlc", "work_ocamlc", env_or("OCAMLC", "ocamlc"), ocaml_src,
               dir);
  } else {
    note_skip("ocamlopt/ocamlc", afmt("no %s", ocaml_src));
  }

  /* Fixed JIT overhead, for interpreting the numbers: the same driver on a
   * workload whose main does nothing. Kept out of g_vars so it is not part
   * of the result-equivalence check (it exits 0, not the workload's value). */
  {
    char *nul = afmt("%s/null.ml", dir);
    FILE *f = fopen(nul, "w");
    if (f != NULL) {
      fputs("let main = 0\n", f);
      fclose(f);
    }
    variant startup = { .label = "jit startup", .min_ms = 1e30 };
    char *av[] = { (char *)driver, nul, "--target", "jit", NULL };
    set_av(&startup, av, 4);
    run_variant(&startup, reps);
    printf("%-24s %10.1f %10.1f %8d  (empty main; fixed cost)\n", startup.label,
           startup.min_ms, startup.mean_ms, startup.exit_code);
    printf("\n");
  }

  printf("%-24s %10s %10s %8s\n", "variant", "min(ms)", "mean(ms)", "exit");
  for (int i = 0; i < g_ng; ++i) {
    if (g_vars[i].enabled) {
      run_variant(&g_vars[i], reps);
      print_row(&g_vars[i]);
    }
  }

  for (int i = 0; i < g_nskips; ++i) {
    printf("skipped: %s\n", g_skips[i]);
  }

  /* Equivalence check over enabled variants. */
  int bad = 0;
  int ref = 0, have_ref = 0;
  for (int i = 0; i < g_ng; ++i) {
    if (!g_vars[i].enabled || !g_vars[i].check_exit) {
      continue;
    }
    if (g_vars[i].exit_code < 0) {
      fprintf(stderr, "bench: %s died from a signal\n", g_vars[i].label);
      bad = 1;
      continue;
    }
    if (!have_ref) {
      ref = g_vars[i].exit_code;
      have_ref = 1;
    } else if (g_vars[i].exit_code != ref) {
      fprintf(stderr, "bench: exit code mismatch: %s → %d, expected %d\n",
              g_vars[i].label, g_vars[i].exit_code, ref);
      bad = 1;
    }
  }
  if (!bad) {
    printf("\nall variants agree on the result (exit %d)\n", ref);
    const variant *jit = NULL, *spec = NULL;
    for (int i = 0; i < g_ng; ++i) {
      if (g_vars[i].enabled && strcmp(g_vars[i].label, "jit") == 0) {
        jit = &g_vars[i];
      }
      if (g_vars[i].enabled && strcmp(g_vars[i].label, "jit+specialize") == 0) {
        spec = &g_vars[i];
      }
    }
    if (jit != NULL && spec != NULL) {
      printf("specialize speedup over jit: %.2fx\n", jit->min_ms / spec->min_ms);
    }
  }

  return bad;
}
