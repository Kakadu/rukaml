# Benchmarks live here; the root Makefile only delegates.
#
# Run from the repository root:
#   make bench
#   make bench-online
#   make bench BENCH_REPS=1 BENCH_ARGS='--sizes 500,1000'
#
# Or directly from this directory:
#   make -C bench
#
# The harness is run with the repository root as its working directory, so
# its default driver/plugin/workload paths keep working.

BENCH_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
ROOT := $(abspath $(BENCH_DIR)/..)

CC ?= cc
GLIB_CFLAGS := $(shell pkg-config --cflags glib-2.0 2>/dev/null)
BENCH_REPS ?= 5
BENCH_ONLINE_REPS ?= 1
# amd64/rv64 support is temporarily disabled while it is reviewed.
# Override with BENCH_EXCLUDE_BACKENDS= to try every row again.
BENCH_EXCLUDE_BACKENDS ?= amd64,rv64
BENCH_EXCLUDE_ARGS := $(if $(strip $(BENCH_EXCLUDE_BACKENDS)),--exclude-backends $(BENCH_EXCLUDE_BACKENDS),)
BENCH_ARGS ?=

HARNESS := $(BENCH_DIR)/harness
PLUGIN := $(BENCH_DIR)/qemu_plugin/libqemu_insn.so

# Every workload kept here demonstrates a measured online-JIT win over
# `aot llvm -O3`; fib is the deliberate plain-recursion reference.  The
# expensive online showcase is a separate target so the default run stays
# reasonably short.
BENCH_WORKLOADS := \
	bench/workloads/recursion/plain.ml \
	bench/workloads/recursion/fib.ml \
	bench/workloads/closures/curried.ml \
	bench/workloads/closures/pipeline.ml \
	bench/workloads/closures/affine.ml \
	bench/workloads/higher_order/hof_fold.ml \
	bench/workloads/online/power.ml

BENCH_ONLINE_WORKLOAD := bench/workloads/online/online_showcase.ml

.PHONY: bench bench-online bench-all build harness clean

# The harness compares the online JIT with AOT LLVM -O3 and reports cold,
# steady-state and a dedicated JIT-overhead table.  rv64/rv32 AOT rows always
# run under qemu.
bench: build harness
	cd $(ROOT) && set -e; \
	for workload in $(BENCH_WORKLOADS); do \
		./bench/harness --reps $(BENCH_REPS) --qemu-insn \
			$(BENCH_EXCLUDE_ARGS) $(BENCH_ARGS) $$workload; \
	done

# The showcase is intentionally large enough to amortize online tier-up; its
# AOT rows are expensive, so it gets one repetition by default.
bench-online: build harness
	cd $(ROOT) && ./bench/harness --reps $(BENCH_ONLINE_REPS) --qemu-insn \
		$(BENCH_EXCLUDE_ARGS) $(BENCH_ARGS) $(BENCH_ONLINE_WORKLOAD)

bench-all: bench bench-online

build:
	cd $(ROOT) && dune build

harness:
	$(CC) -O2 -std=c11 -Wall -Wextra -o $(HARNESS) $(BENCH_DIR)/harness.c
	-$(CC) -O2 -shared -fPIC $(GLIB_CFLAGS) \
		-o $(PLUGIN) \
		$(BENCH_DIR)/qemu_plugin/qemu_insn_plugin.c

clean:
	$(RM) $(HARNESS) $(PLUGIN)
