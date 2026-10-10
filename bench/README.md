# JIT vs AOT benchmark

`harness.c` runs one rukaml workload through every available execution path and
reports wall-clock time:

| variant | what runs | timed region |
| --- | --- | --- |
| `jit` | `driver --target jit` | exec + parse/infer/ANF + JIT compile + run |
| `jit+specialize` | `driver --target jit --opt=specialize` | exec + the above + IR patch + run |
| `aot llvm` | `driver --target llvm` → `clang-19 -O2` | exec + run (binary prepared up front) |
| `aot amd64` | `driver --target amd64` → `nasm` + `gcc` | exec + run (binary prepared up front) |
| `aot rv64` | `driver --target rv64` → riscv gcc, run under `qemu-riscv64` | exec + run (binary prepared up front) |
| `ocamlopt` | `ocamlopt` on the companion `<workload>.ocaml.ml` | exec + run (binary prepared up front) |
| `ocamlc` | `ocamlc` on the companion `<workload>.ocaml.ml` | exec + run (bytecode, prepared up front) |

All AOT binaries and the specialization profile are produced **before** timing,
so the only difference between the rows is the run itself (the JIT rows also
pay for their own compilation; the `jit startup` line estimates that fixed cost
with an empty `main`). A backend whose toolchain is not installed is skipped
with a note rather than failing the run.

The harness checks that all enabled rows exit with the same code (the
workload's result) and fails otherwise, so a "fast" run that computed something
else is reported as invalid. Two of the rows need a word of explanation:

* `rv64` uses a **different calling convention** from the other rukaml
  backends: it emits direct `call`s and never touches
  `rukaml_alloc_closure`/`rukaml_applyN`, so it pays no closure/allocation cost
  at all. It is reported for completeness but is not directly comparable to the
  closure-based rows.
* `rv64` is also **freestanding**: its generated `main` ends with
  `addi a0, x0, 0; addi a7, x0, 93; ecall` and therefore always exits 0. To let
  the row take part in the result check, the harness rewrites that one
  instruction so `a0` (which already holds `main`'s value) becomes the exit
  status. The computation is untouched; if the expected sequence is not found
  the row is timed but dropped from the check.

## Usage

From the repo root, after `dune build` / `make build`:

```sh
make bench                                  # compiles the harness, runs every workload
# or directly:
cc -O2 -std=c11 -Wall -Wextra -o bench/harness bench/harness.c
./bench/harness bench/workloads/curried.ml 5
```

Environment overrides:

```
RUKAML_DRIVER                 (default _build/default/driver/driver.exe)
OCAMLOPT / OCAMLC             (default ocamlopt / ocamlc)

CC_LLVM  AS_LLVM  AS_FLAGS_LLVM  LD_LLVM  LD_FLAGS_LLVM  RUN_LLVM   (default clang-19)
CC_AMD64 AS_AMD64 AS_FLAGS_AMD64 LD_AMD64 LD_FLAGS_AMD64 RUN_AMD64  (default cc / nasm)
CC_RV64  AS_RV64  AS_FLAGS_RV64  LD_RV64  LD_FLAGS_RV64  RUN_RV64   (default riscv64-linux-gnu-gcc)
RUKAML_RUNTIME_{LLVM,AMD64,RV64}  (default back/<tgt>/rukaml_stdlib.c)
```

The `amd64`/`rv64` toolchains mirror `testsuite/build.ml`: the runtime is
compiled with `CC_<tgt>`, the emitted `.s` is assembled with `AS_<tgt>` /
`AS_FLAGS_<tgt>`, the two objects are linked with `LD_<tgt>`, and the result is
run with the `RUN_<tgt>` prefix (empty for native targets, `qemu-riscv64 -L
/usr/riscv64-linux-gnu` for rv64).

## Workloads

* `workloads/plain.ml` — a two-argument tail-recursive sum: no closures, so
  the `specialize` pass has nothing to do. Baseline.
* `workloads/curried.ml` — partial application of `add` inside a hot loop,
  i.e. a fresh `alloc_closure -> applyN` chain per iteration. This is the
  shape `specialize` rewrites into a direct call to `add$spec$0` (constant `1`
  baked in), so it shows the pass's effect (≈1.7× over the plain JIT).
* `workloads/pipeline.ml` — four unary transforms partially applied to a
  constant and chained inside a hot inner loop (≈4.1× over the plain JIT).
* `workloads/affine.ml` — three binary `fma`s partially applied to *both* of
  their constants inside a hot inner loop, i.e. longer
  `alloc_closure -> applyN -> applyN -> applyN` chains per iteration (≈4.6×).
* `workloads/*.ocaml.ml` — OCaml twins of the four workloads. They compute the
  same checksum and pass it to `exit`, so the exit-code check covers them too.
  OCaml turns the tail calls into loops, so no stack-limit trick is needed
  there; `ocamlc` bytecode is also reported to show the native/bytecode spread.

`plain.ml` and `curried.ml` recurse a million times. rukaml emits no `tail`
markers, so neither the JIT nor `clang -O2` turns that recursion into a loop and
it needs hundreds of MiB of stack — the harness raises `RLIMIT_STACK` to the
hard limit before forking. `pipeline.ml` and `affine.ml` instead put the hot
work in a nested loop whose depth stays around 2000 frames; that keeps the
inner loop's code cache-resident and lets the per-iteration transform cost,
not stack traffic, dominate.

## Reference numbers

Measured 2026-10-10 on the dev machine (LLVM 19, `clang-19`, gcc 16, OCaml
5.3.0), 5 reps, `min`:

```
plain.ml     jit  295.8  jit+specialize  303.3  aot llvm  345.4  aot amd64  269.0  aot rv64   46.5  ocamlopt 1.9  ocamlc 11.6  (exit 32)
curried.ml   jit  512.0  jit+specialize  301.2  aot llvm  533.3  aot amd64  425.1  aot rv64   67.5  ocamlopt 2.7  ocamlc 25.0  (exit 64)
pipeline.ml  jit 1016.5  jit+specialize  250.6  aot llvm  912.6  aot amd64  695.7  aot rv64  125.4  ocamlopt 1.6  ocamlc 10.9  (exit 64)
affine.ml    jit 1029.3  jit+specialize  225.1  aot llvm  945.2  aot amd64  807.7  aot rv64  112.1  ocamlopt 1.6  ocamlc 11.4  (exit 192)
jit startup  14.5 ms (empty main)
```

* On plain recursion the JIT's `Default` codegen (no IR pipeline) is
  competitive with `clang -O2`; both do 2×`rukaml_applyN` + an
  `alloc_closure` per iteration (the fully-applied fast path in `LLVM_impl`
  does not fire for this shape), which is why a million iterations cost
  hundreds of ms. The native `amd64` backend is fastest of the closure-based
  rukaml paths, and `specialize` has nothing to remove here (≈1.0×).
* On the workloads with constant partial applications, `specialize` removes
  the per-iteration closure allocation and the varargs dispatches, turning
  each chain into a direct call to a clone with the constants folded in:
  1.7× (curried), 4.1× (pipeline), 4.6× (affine) over the plain JIT, and
  `jit+specialize` ends up faster than `clang -O2`, which sees the closure as
  an opaque function pointer and cannot devirtualize it. `affine` wins more
  per site than `pipeline` because its chains are longer (two constants baked
  into a three-argument function).
* `rv64` is the fastest rukaml path here, but for a different reason: it uses
  direct calls (see above) and, under `qemu-riscv64`, its timing is emulated
  rather than native. Treat it as a calling-convention comparison, not a
  same-ABI speed comparison.
* OCaml is ~100–600× faster on the rukaml rows: `ocamlopt` turns the tail
  calls into loops where rukaml pays a closure allocation and two varargs
  dispatches per iteration. This is a codegen / calling-convention gap, not a
  measurement artifact.
* The JIT costs ~15 ms of fixed startup/compilation — meaningful next to the
  few-ms OCaml rows, negligible next to rukaml's hundred-millisecond runs.

## rv64 prerequisites

On Arch (this machine) `qemu-riscv64` comes from `qemu-user` and the cross
compiler, binutils and glibc sysroot from `extra`:

```sh
sudo pacman -S riscv64-linux-gnu-gcc   # pulls binutils + glibc (/usr/riscv64-linux-gnu)
```

On Debian/Ubuntu the equivalent is `gcc-riscv64-linux-gnu` plus the
`libc6-riscv64-cross` sysroot. With the tools in place the harness picks up the
`aot rv64` row automatically; the run prefix is overridable with `RUN_RV64`.
