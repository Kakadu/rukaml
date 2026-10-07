#include "rukaml_orc_jit.hpp"

#include "llvm/Support/Error.h"
#include "llvm/Support/TargetSelect.h"

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

extern "C" {
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

// Rukaml LLVM runtime (back/llvm/rukaml_stdlib.c) compiled to the same .so
extern "C" {
void myputc(int x);
void *rukaml_apply0(void *f);
void *rukaml_apply1(void *f, void *arg1);
void *rukaml_apply2(void *f, void *arg1, void *arg2);
void *rukaml_apply3(void *f, void *arg1, void *arg2, void *arg3);
void *rukaml_alloc_pair(void *l, void *r);
void *rukaml_field(int n, void **r);
void *rukaml_alloc_closure(void *func, int32_t argsc);
void *rukaml_applyN(void *f, int32_t argc, ...);
}

static llvm::orc::RukamlJIT *g_jit = nullptr;
static std::string g_err; // global: it has no destructor at the moment of longjmp

template <typename F> static uint64_t addr_of(F f) {
  return (uint64_t)(uintptr_t)f;
}

// Runtime symbol names must match declare_primitive in
// back/llvm/LLVM_impl.ml (build_module) and the definitions in rukaml_stdlib.c.
static const std::vector<std::pair<std::string, uint64_t>> k_runtime_syms = {
    {"myputc", addr_of(&myputc)},
    {"rukaml_apply0", addr_of(&rukaml_apply0)},
    {"rukaml_apply1", addr_of(&rukaml_apply1)},
    {"rukaml_apply2", addr_of(&rukaml_apply2)},
    {"rukaml_apply3", addr_of(&rukaml_apply3)},
    {"rukaml_alloc_pair", addr_of(&rukaml_alloc_pair)},
    {"rukaml_field", addr_of(&rukaml_field)},
    {"rukaml_alloc_closure", addr_of(&rukaml_alloc_closure)},
    {"rukaml_applyN", addr_of(&rukaml_applyN)},
};

extern "C" CAMLprim value rukaml_orc_create(value unit) {
  CAMLparam1(unit);

  g_err.clear();
  if (!g_jit) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    auto jit_or_error = llvm::orc::RukamlJIT::create();
    if (!jit_or_error)
      g_err = llvm::toString(jit_or_error.takeError());
    else
      g_jit = jit_or_error->release();

    // The runtime is registered exactly once — here, when g_jit is
    // actually created: a second create without it would be a duplicate
    // symbol definition, and registering outside of create belongs to the
    // backend, not the driver.
    if (g_jit) {
      if (llvm::Error err = g_jit->define_abs_symbols(k_runtime_syms)) {
        g_err = llvm::toString(std::move(err));
        delete g_jit; // let the next create retry everything from scratch
        g_jit = nullptr;
      }
    }
  }

  if (!g_err.empty())
    caml_failwith(g_err.c_str());

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value rukaml_orc_update(value ir_module) {
  CAMLparam1(ir_module);

  if (!g_jit)
    caml_failwith("JIT not created. Call create() first.");

  g_err.clear();
  {
    // a copy is needed: the OCaml string may be moved by the GC
    std::string ir(String_val(ir_module), caml_string_length(ir_module));
    if (llvm::Error err = g_jit->add_ir(ir))
      g_err = llvm::toString(std::move(err));
  } // ir is destroyed here, before caml_failwith

  if (!g_err.empty())
    caml_failwith(g_err.c_str());

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value rukaml_orc_run(value name) {
  CAMLparam1(name);

  if (!g_jit)
    caml_failwith("JIT not created. Call create() first.");

  g_err.clear();
  int64_t rez = 0;
  {
    // a copy is needed: the OCaml string may be moved by the GC
    std::string sym(String_val(name), caml_string_length(name));
    auto addr = g_jit->lookup(sym);
    if (!addr)
      g_err = llvm::toString(addr.takeError());
    else
      // every rukaml function compiles to i64 (...) -> i64
      rez = addr->getAddress().toPtr<int64_t (*)()>()();
  }

  if (!g_err.empty())
    caml_failwith(g_err.c_str());

  CAMLreturn(caml_copy_int64(rez));
}

extern "C" CAMLprim value rukaml_orc_destroy(value unit) {
  CAMLparam1(unit);

  // ~RukamlJIT runs endSession(); delete nullptr is safe, so a repeated
  // destroy (and destroy without create) is a no-op.
  delete g_jit;
  g_jit = nullptr;

  CAMLreturn(Val_unit);
}
