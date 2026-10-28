# Run from the repository root:
#   make bench
#   make bench-online
#   make bench BENCH_REPS=1 BENCH_ARGS='--sizes 500,1000'

BENCH_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
ROOT := $(abspath $(BENCH_DIR)/../..)
HARNESS := $(BENCH_DIR)/harness

CC ?= cc
BENCH_REPS ?= 5
BENCH_ONLINE_REPS ?= 1
BENCH_ARGS ?=

BENCH_WORKLOADS := \
	jit/bench/workloads/recursion/plain.ml \
	jit/bench/workloads/recursion/fib.ml \
	jit/bench/workloads/closures/curried.ml \
	jit/bench/workloads/closures/pipeline.ml \
	jit/bench/workloads/closures/affine.ml \
	jit/bench/workloads/higher_order/hof_fold.ml \
	jit/bench/workloads/online/power.ml \
	jit/bench/workloads/online/tre_devirt.ml \
	jit/bench/workloads/online/spec_nonlinear.ml

BENCH_ONLINE_WORKLOAD := jit/bench/workloads/online/online_showcase.ml

.PHONY: bench bench-online bench-all build harness

bench: build harness
	cd $(ROOT) && set -e; \
	for workload in $(BENCH_WORKLOADS); do \
		$(HARNESS) --reps $(BENCH_REPS) $(BENCH_ARGS) $$workload; \
	done

bench-online: build harness
	cd $(ROOT) && $(HARNESS) --reps $(BENCH_ONLINE_REPS) $(BENCH_ARGS) $(BENCH_ONLINE_WORKLOAD)

bench-all: bench bench-online

build:
	cd $(ROOT) && dune build

harness: $(HARNESS)

$(HARNESS): $(BENCH_DIR)/harness.c
	$(CC) -O2 -std=c11 -Wall -Wextra -o $@ $<
