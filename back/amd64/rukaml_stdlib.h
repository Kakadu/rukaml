#ifndef RUKAML_STDLIB_H
#define RUKAML_STDLIB_H

#include <stdint.h>

void rukaml_initialize(uint64_t ebp, int argc, char **argv);

void dfs(uint64_t *allocated, uint64_t *root);

void rukaml_gc_compact(uint64_t rsp);
void rukaml_gc_print_stats(void);
void rukaml_print_alloc_closure_count(void);

void rukaml_print_int(int64_t);
void rukaml_print_int_kaml(int, int, int, int, int, int, int64_t);

void **rukaml_array_stdin(void);

void *rukaml_stdin(void);
void *rukaml_stdout(void);
void *rukaml_stderr(void);

void *rukaml_open_in(int, int, int, int, int, int, void **path);
void *rukaml_open_out(int, int, int, int, int, int, void **path);
void rukaml_close_channel(int, int, int, int, int, int, void *channel);

int64_t rukaml_input_char(int, int, int, int, int, int, void *channel);
int64_t rukaml_end_of_input(int, int, int, int, int, int, void *channel);

void **rukaml_string_of_char_list(int, int, int, int, int, int, void **chs);
bool rukaml_string_equal(int, int, int, int, int, int, void **left, void **right);
char rukaml_string_nth(int, int, int, int, int, int, void **str, uint64_t n);

void *rukaml_alloc_printf_closure(int, int, int, int, int, int, void **fmt);
void *rukaml_alloc_fprintf_closure(int, int, int, int, int, int, void *out_channel, void **fmt);
void *rukaml_alloc_sprintf_closure(int, int, int, int, int, int, void **fmt);

void *rukaml_alloc_block(uint64_t size, uint64_t tag);

uint64_t rukaml_block_size(int, int, int, int, int, int, void **obj);
uint64_t rukaml_block_tag(int, int, int, int, int, int, void **obj);
void *rukaml_block_nth(int, int, int, int, int, int, void **obj,
                       uint64_t n);

void *rukaml_field(void **obj, uint64_t n);

uint64_t rukaml_equal_struct(void **left, void **right);

void rukaml_array_set(int, int, int, int, int, int, void **arr, uint64_t n,
                      void *a);

void rukaml_match_failure();

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

void *rukaml_alloc_closure(void *func, int32_t argsc);

void *rukaml_applyN(void *f, int64_t argc, ...);

void *rukaml_argv(void);

#endif // RUKAML_STDLIB_H
