# Benchmarks

`jit/bench/harness.c` runs one rukaml workload through four programs:

| row | how it is built | role |
| --- | --- | --- |
| `jit` | `driver --target jit --opt=online` | the online JIT under test |
| `aot llvm -O3` | `driver --target llvm`, `clang-19 -O3` | baseline |
| `ocamlc` | the OCaml twin, `ocamlc` (bytecode) | reference |
| `ocamlopt` | the OCaml twin, `ocamlopt` (native) | reference |

The OCaml twins live in `jit/bench/workloads/ocaml/<theme>/<name>.ocaml.ml` and
mirror `jit/bench/workloads/<theme>/<name>.ml`. Every row must return the same
exit status as the baseline.

## Running

Run from the repository root (`eval $(opam env)` first if `_opam/bin` is not in
`PATH`):

```sh
make bench                                          # default workloads, 5 reps
make bench-online                                   # online showcase, 1 rep
make bench BENCH_REPS=1 BENCH_ARGS='--sizes 500'    # quick smoke run
./jit/bench/harness --reps 3 --verbose jit/bench/workloads/recursion/plain.ml
```

`make bench-online` uses `BENCH_ONLINE_REPS` (default `1`). The showcase is not
part of `make bench` because its AOT rows take several seconds per size.

Options:

| option | meaning |
| --- | --- |
| `--reps N` | timed repetitions per row (default 5, max 64) |
| `--sizes N0,N1,...` | sizes for the steady fit (default `base/2,base,2*base`) |
| `--only PAT` | run only rows whose label contains `PAT` |
| `--cpu N` | pin the children to CPU `N` (also `RUKAML_BENCH_CPU=N`) |
| `--json FILE` | write the results as JSON |
| `--verbose` | print the median of every row and size |

Environment: `RUKAML_DRIVER` (default `_build/default/driver/driver.exe`),
`CC_LLVM` (default `clang-19`), `CC` for the harness build.

## Output

* **cold** — the whole process lifetime at the base size. The `vs llvm` column
  is the ratio to `aot llvm -O3`.
* **steady** — the slope `b` and intercept `a` of `T(N) = a + b*N` over the
  sizes, `N` being the outer iteration count. The slope is shown only when it
  is positive and `R^2 >= 0.5`; otherwise `n/a`, which usually means the
  online JIT body is below timing noise.
* **JIT overhead** — `jit` minus `aot llvm -O3`: cold, fit intercept, fit
  slope, empty-main startup, a compile-only probe (the workload with
  `let main = 0`), and the number of delta modules compiled by online tier-up.
  Positive means the JIT is slower.
* **result check** — every row at the base size must match the baseline's exit
  status.

Reading the results: `cold` includes compilation, so for short runs the native
OCaml compiler can beat the JIT even when the JIT beats `aot llvm -O3`. The
steady slope and the tier-up count show whether the JIT wins on the loop body
itself.

## Workloads

Each workload binds `let bench_iters = N`. The harness removes that binding and
substitutes the literal everywhere else, because a top-level integer is a
nullary function in rukaml: passing the bare name would pass a closure.

| workload | what it stresses |
| --- | --- |
| `recursion/plain.ml` | plain recursion, no closures |
| `recursion/fib.ml` | two-argument recursive Fibonacci |
| `closures/curried.ml` | one partial application per iteration |
| `closures/pipeline.ml` | four unary partial applications per iteration |
| `closures/affine.ml` | three binary partial applications per iteration |
| `higher_order/hof_fold.ml` | a closure parameter called through `applyN` |
| `online/power.ml` | runtime-constant arguments in a hot loop |
| `online/tre_devirt.ml` | a self-recursive accumulator: devirt + tail-call elimination |
| `online/spec_nonlinear.ml` | runtime-constant specialization without a closed form |
| `online/online_showcase.ml` | runtime closure, mode, and partial application (`make bench-online`) |

`online/spec_linear.ml` is kept for reference but is not in `make bench`: its
AOT row crashes at the default size because `outer` is not tail-recursive.

## Notes

* `outer` recursion depth equals `bench_iters` in some workloads; the harness
  raises `RLIMIT_STACK` for the children.
* Children run with address-space randomization disabled.
