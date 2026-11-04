#ifndef RUKAML_LLVM_H
#define RUKAML_LLVM_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

void myputc(int x);

void rukaml_print_int(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                      uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                      int x);

void *rukaml_apply0(void *f);
void *rukaml_apply1(void *f, void *arg1);
void *rukaml_apply2(void *f, void *arg1, void *arg2);
void *rukaml_apply3(void *f, void *arg1, void *arg2, void *arg3);
void *rukaml_apply4(void *f, void *arg1, void *arg2, void *arg3, void *arg4);

void *rukaml_alloc_pair(void *l, void *r);
void *rukaml_alloc_array(size_t *arr, uint64_t len);
void *rukaml_field(int n, void **r);
void *rukaml_alloc_closure(void *func, int32_t argsc);
void *rukaml_applyN(void *f, int32_t argc, ...);

uint64_t rukaml_array_length(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                             uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                             void **arr);
void *rukaml_array_get(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                       uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                       void **arr, uint64_t n);
void rukaml_array_set(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                      uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                      void **arr, uint64_t n, void *a);

void rukaml_prof_tick(const char *name);
void rukaml_prof_dump(const char *path);

#ifdef __cplusplus
}
#endif

#endif /* RUKAML_LLVM_H */
