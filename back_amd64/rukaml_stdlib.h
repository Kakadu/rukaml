#ifndef RUKAML_STDLIB_H
#define RUKAML_STDLIB_H

#include <stdint.h>

void rukaml_initialize(uint64_t ebp);

void dfs(uint64_t *allocated, uint64_t *root);

void rukaml_gc_compact(uint64_t rsp);
void rukaml_gc_print_stats(void);

void rukaml_print_int(int,int,int,int,int,int,int x);

typedef void *(*fun0)(void);
typedef void *(*fun1)(void *);
typedef void *(*fun2)(void *, void *);
typedef void *(*fun3)(void *, void *, void *);
typedef void *(*fun7)(void *, void *, void *, void *, void *, void *, void *);
typedef void *(*fun8)(void *, void *, void *, void *, void *, void *, void *, void *);
typedef void *(*fun9)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void *(*fun10)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void *(*fun11)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void *(*fun12)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

void *rukaml_apply0(fun0 f);

// NOTE: Below we pass first 6 arguments as zeros, because they go to the registers.
// Others will go on stack
void *rukaml_apply1(fun7 foo, void *arg1);
void *rukaml_apply2(fun8 f, void *arg1, void *arg2);

void *rukaml_alloc_pair(void *l, void *r);

void *rukaml_field(int n, void **r);
void *rukaml_alloc_closure(void *func, int32_t argsc);

void *rukaml_applyN(void *f, int64_t argc, ...);

#endif // RUKAML_STDLIB_H
