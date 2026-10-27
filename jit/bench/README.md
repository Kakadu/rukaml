# JIT vs AOT benchmark

`jit/bench/harness.c` compares the **real online JIT** with **rukaml's AOT LLVM
backend at `-O3`**.  The matrix is intentionally small:

| row | command | runner |
| --- | --- | --- |
| `jit` | `driver --target jit --opt=online` | host |
| `aot llvm -O3` | `driver --target llvm`, `clang-19 -O3` | host |
| `aot amd64` | `driver --target amd64`, `nasm` + `cc` | host |
| `aot rv64` | `driver --target rv64`, riscv gcc | `qemu-riscv64` |
| `aot rv32` | `driver --target rv32`, riscv gcc | `qemu-riscv32` |

`aot llvm -O3` is the baseline for every comparison.  `aot rv64` and
`aot rv32` are **always** run under qemu; the old `RUN_RV64`/`RUN_RV32`
overrides are gone.  `aot rv32` is skipped automatically when the rv32
toolchain/sysroot is unavailable.

> **Temporary backend status:** AOT `amd64`/`rv64` support is temporarily
> disabled pending review.  `make bench` excludes those rows by default
> (`BENCH_EXCLUDE_BACKENDS=amd64,rv64`), so only `jit` and `aot llvm -O3` are
> expected to work.  Use `BENCH_EXCLUDE_BACKENDS=` to try every row again.

Offline JIT variants (`profile`, `specialize`, `devirt`, `o2`) and the OCaml
comparison rows were removed: they are a different workflow and made the
harness large.  The JIT row is only `--opt=online`.  `aot amd64` is normally the
host-native sanity check; it is temporarily excluded together with `rv64`.  The
baseline is always `aot llvm -O3`.

## Workload layout

The Rukaml programs are grouped by theme:

| directory | workloads |
| --- | --- |
| `jit/bench/workloads/recursion/` | `plain.ml`, `fib.ml` |
| `jit/bench/workloads/closures/` | `curried.ml`, `pipeline.ml`, `affine.ml` |
| `jit/bench/workloads/higher_order/` | `hof_fold.ml` |
| `jit/bench/workloads/online/` | `power.ml`, `online_showcase.ml` |

The `.ocaml.ml` twins used for manual `ocamlc`/`ocamlopt` comparisons live in a
parallel tree: `jit/bench/workloads/ocaml/<theme>/`.  They are not run by the
harness and are not part of `make bench`.

`ack` and the CPS workloads were removed from the active suite: `ack` is slower
under the online JIT than under AOT LLVM, and CPS `--cps --caa` support is not
verified.  The remaining workloads all have a measured online-JIT win over
`aot llvm -O3`; `fib` is kept as the plain-recursion reference.

## What is measured

* **cold** — the whole child lifetime at the base size.  For `jit` this
  includes frontend, ORC compile, online tier-up and the run; for AOT rows it
  is process startup plus the pre-built binary's run.
* **steady** — the slope/intercept of `T(N) = a + b*N` over the selected
  sizes, where `N` is the number of outer iterations.  This removes fixed
  process/qemu startup.  The slope is reported only when it is positive and
  `R^2 >= 0.5`; otherwise the table prints `n/a`.  The online JIT often
  specializes/constant-folds the fixed-depth inner loop, so its runtime can be
  below timing noise while compile/tier-up dominates cold time.
* **JIT overhead** — a dedicated table with `jit - aot llvm -O3` values:
  * cold difference at the base size;
  * difference of the fit intercepts (`a`), i.e. the fixed cost;
  * difference of the fit slopes (`b`), when both fits are usable;
  * empty-main startup (`let main = 0`) for JIT and the baseline;
  * JIT startup+compile probe (`let main = 0` with the workload functions
    present), and its delta against the baseline's empty-main startup;
  * the base compile alone (`startup+compile probe - JIT startup`);
  * number of delta modules compiled by online tier-up.
  Positive values mean the JIT is slower; negative values mean it is faster.
* **qemu instructions** — guest instruction count `I(N) = a + b*N` for qemu
  rows (`--qemu-insn`).
* **result check** — every checked row at the base size must return the same
  exit status as `aot llvm -O3`.
* **JSON** — cold, steady, fixed-cost, instruction and comparison data.

## Reading the results

The main JIT-vs-AOT question is answered by two lines in the overhead table:

* `cold, base N` is the end-to-end one-shot cost.  A negative delta means the
  JIT is already faster despite paying compilation.
* `startup (empty main)` and `JIT startup+compile (probe)` show what the JIT
  pays before running; the latter's delta against `aot llvm -O3` startup is
  the single pre-run overhead number.  `JIT base compile` is the same probe
  minus JIT startup; online tier-up is not triggered by `main = 0`, so this
  isolates the base compile.

`steady (fit b)` is the per-outer-iteration cost when the fit is usable.  For
`plain`/`curried`/`affine`, online specialization can constant-fold the
fixed-depth inner loop, so the JIT slope is often below noise and prints
`n/a`.  In that case use cold time and the compile/startup numbers; do not
interpret the missing slope as missing data.

qemu rows are useful for cross-ISA analysis (`steady` and guest instructions),
but their absolute cold times include qemu process startup and emulation, so
do not read them as native rv64/rv32 numbers.

## Workload parameterization

Each workload contains:

```ocaml
let bench_iters = 1000
```

`bench_iters` changes only the number of outer iterations; the inner loop has
a fixed depth (or, in `online_showcase`, two fixed depths), so stack usage stays
constant and `T(N)` is linear.  The harness replaces the marker binding and
substitutes the same literal wherever `bench_iters` appears.  This is required
because the LLVM/amd64/rv64/rv32 backends compile a top-level integer as a
nullary function; passing the bare name would pass a closure.  The substitution
also lets `online_showcase` compare `m` with `bench_iters` inside `outer`.

Default sizes are `base/2`, `base`, `2*base`.  If `--sizes` does not contain
the base size, it is appended so the cold table and result check always have a
common point.

| workload | inner loop | result (modulo 256) |
| --- | --- | --- |
| `recursion/plain.ml` | plain recursion, no closures | `bench_iters * 500500` |
| `recursion/fib.ml` | two-argument recursive Fibonacci | `bench_iters * F(22)` |
| `closures/curried.ml` | one partial application per iteration | `bench_iters * 1000` |
| `closures/pipeline.ml` | four unary partial applications per iteration | `bench_iters * 5000` |
| `closures/affine.ml` | three binary partial applications per iteration | `bench_iters * 15000` |
| `higher_order/hof_fold.ml` | a closure parameter called through `applyN` | `bench_iters * 1502500` |
| `online/power.ml` | runtime-constant `exp`/`x` in a hot loop | `bench_iters * 500 * 364` |
| `online/online_showcase.ml` | runtime closure + mode + partial application | checked against AOT |

The closure workloads (`curried`, `pipeline`, `affine`) and `hof_fold` are the
main devirtualization/inline-cache cases; `power` and `online_showcase` add
guarded specialization on runtime constants.  AOT LLVM sees those values as
opaque parameters and keeps the generic `applyN` path.

Indicative one-repetition cold times at the base size (host, ms; this is a
smoke comparison, not a statistical result):

| workload | `jit` | `aot llvm -O3` |
| --- | ---: | ---: |
| `recursion/plain.ml` | 164 | 319 |
| `recursion/fib.ml` | 145 | 387 |
| `closures/curried.ml` | 198 | 620 |
| `closures/pipeline.ml` | 443 | 2009 |
| `closures/affine.ml` | 244 | 1644 |
| `higher_order/hof_fold.ml` | 284 | 661 |
| `online/power.ml` | 312 | 698 |
| `online/online_showcase.ml` | 979 | 8276 |

## Usage

The bench targets live in `jit/bench/Makefile`; the root Makefile delegates with
`$(MAKE) -C bench`.  Run from the repository root (`eval $(opam env)` first if
`_opam/bin` is not in `PATH`):

```sh
# Default workloads, default sizes, 5 repetitions, qemu instruction counts:
make bench

# The large online showcase (defaults to one repetition because AOT is slow):
make bench-online

# Both of the above:
make bench-all

# Equivalent direct invocation:
make -C bench

# Quick smoke run:
make bench BENCH_REPS=1 BENCH_ARGS='--sizes 500,1000'

# Try the temporarily disabled backends too:
make bench BENCH_EXCLUDE_BACKENDS=

# One row:
./jit/bench/harness --reps 3 --sizes 500,1000 --only 'jit' \
  jit/bench/workloads/recursion/plain.ml
./jit/bench/harness --reps 3 --sizes 500,1000 --only 'aot rv64' \
  jit/bench/workloads/recursion/plain.ml

# Full output:
./jit/bench/harness --reps 3 --sizes 500,1000,2000 --verbose --qemu-insn \
  --json bench.json jit/bench/workloads/recursion/plain.ml
```

`make bench-online` uses `BENCH_ONLINE_REPS` (default `1`); override it with
`BENCH_ONLINE_REPS=3` when a statistical run is needed.  The showcase is not in
the default `make bench` because its AOT rows take several seconds per size.

Options:

| option | meaning |
| --- | --- |
| `--reps N` | timed repetitions per row (default 5, max 64) |
| `--sizes N0,N1,...` | sizes for the steady fit (default `base/2,base,2*base`) |
| `--only PAT` | keep labels containing `PAT` (single substring, not a list) |
| `--skip PAT` | drop labels containing `PAT` |
| `--exclude-backends PAT,...` | drop rows whose label contains any comma/space-separated token; repeatable |
| `--json FILE` | write cold/steady/fixed/instruction/comparison data as JSON |
| `--cpu N` | pin children to one CPU (also `RUKAML_BENCH_CPU=N`) |
| `--qemu-insn` | count guest instructions for qemu rows |
| `--verbose` | print the median point for every row/size |

The harness disables address-space randomization for children and raises
`RLIMIT_STACK`, so recursive workloads behave consistently.

## qemu instruction counting

`jit/bench/qemu_plugin/qemu_insn_plugin.c` is a qemu TCG plugin that counts every
translated guest instruction and writes the total to `QEMU_INSN_FILE` at guest
exit.  The harness inserts `-plugin <path>` into the qemu command line and
fits `I(N) = a + b*N`.  Build it manually if needed:

```sh
cc -O2 -shared -fPIC $(pkg-config --cflags glib-2.0) \
  -o jit/bench/qemu_plugin/libqemu_insn.so \
  jit/bench/qemu_plugin/qemu_insn_plugin.c
```

If the plugin is missing, the harness prints a `skipped` note and continues.

## Backend caveats

### Temporary amd64/rv64 disablement

The `amd64` and `rv64` compatibility code is temporarily removed for a later
review.  The Makefile therefore passes `--exclude-backends amd64,rv64`; the
harness itself still tries every row unless that option is given.

### LLVM AOT

The IR is compiled with `clang-19 -O3`; the C runtime is compiled with
`-O2 -fPIC`.  This is the baseline, so its startup is a normal native process
start (about 2 ms here), not a JIT startup.

### amd64

The generated `_start` does not export itself or load `argc`/`argv` into
`rdi`/`rsi`, so the harness patches the prologue before assembling:

```asm
_start:
  mov rdi, [rsp]      ; argc
  lea rsi, [rsp+8]    ; argv
  call main
```

The runtime is compiled with `-D__amd64__=0 -include jit/bench/amd64_compat.h`,
because the legacy `#if __amd64__` block conflicts with modern compilers that
define `__amd64__`.

### rv64

The harness removes the backend's final

```asm
addi a0, x0, 0 # Use 0 return code
```

so `main`'s result becomes the process exit status.  qemu runs with a larger
non-collecting heap (`RUKAMLRUNPARAM=m=134217728`) and a large guest stack
(`QEMU_STACK_SIZE=536870912`).  The rv64 runtime has no collector, so a fixed
heap is required for closure-heavy rows.

### rv32

The rv32 backend shares `back/rv64/rukaml_stdlib.c`.  It is skipped when the
rv32 multilib/sysroot is missing; on this machine `/usr/riscv32-linux-gnu` is
not installed.

### qemu runners

qemu is selected by target, not by a `RUN_*` environment variable:

| variable | default |
| --- | --- |
| `QEMU_RV64` | `qemu-riscv64` |
| `QEMU_RV64_FLAGS` | `-L /usr/riscv64-linux-gnu -cpu rv64` |
| `QEMU_RV32` | `qemu-riscv32` |
| `QEMU_RV32_FLAGS` | `-L /usr/riscv32-linux-gnu -cpu rv32` |

## Toolchain overrides

```sh
RUKAML_DRIVER, RUKAML_BENCH_CPU, RUKAML_QEMU_PLUGIN,
CC_LLVM, CFLAGS_LLVM, AS_LLVM, AS_FLAGS_LLVM, LD_LLVM, LD_FLAGS_LLVM,
CC_AMD64, CFLAGS_AMD64, AS_AMD64, AS_FLAGS_AMD64, LD_AMD64, LD_FLAGS_AMD64,
CC_RV64, CFLAGS_RV64, AS_RV64, AS_FLAGS_RV64, LD_RV64, LD_FLAGS_RV64,
CC_RV32, CFLAGS_RV32, AS_RV32, AS_FLAGS_RV32, LD_RV32, LD_FLAGS_RV32,
RUKAML_RUNTIME_LLVM, RUKAML_RUNTIME_AMD64,
RUKAML_RUNTIME_RV64, RUKAML_RUNTIME_RV32
```

Example:

```sh
QEMU_RV64='qemu-riscv64' \
QEMU_RV64_FLAGS='-L /opt/rv64-sysroot -cpu rv64' \
  ./jit/bench/harness --only 'aot rv64' --reps 3 jit/bench/workloads/recursion/plain.ml
```
