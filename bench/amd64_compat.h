/*
 * Compatibility shim for back/amd64/rukaml_stdlib.c with current compilers.
 *
 * The runtime keeps a legacy `#if __amd64__` block that typedefs `value` and
 * then unconditionally `#define value uint64_t *` right after it.  Modern GCC
 * and clang define `__amd64__`, so that combination is a hard type conflict.
 * Compile the runtime with `-D__amd64__=0` and provide the handful of
 * definitions the block would have supplied.  The representation is identical
 * (`uint64_t *` vs `int64_t *`), so this does not change generated code.
 */
#ifndef RUKAML_BENCH_AMD64_COMPAT_H
#define RUKAML_BENCH_AMD64_COMPAT_H

#include <stdint.h>

#define Int_val(n) ((int64_t)(n))
#define rukaml_word_size 8
#define PRIxVAL "lx"
#define PRIdVAL "ld"

#define HEADER(size, tag) ((uint64_t)(((size) << 10u) + ((tag) % 256u)))
#define SIZE(ptr) (*((uint64_t *)(ptr) - 1) >> 10)
#define TAG(ptr) (*((uint64_t *)(ptr) - 1) & 0xFF)
#define FIELD(ptr, n) ((uint64_t *)(ptr) + (n))

#endif
