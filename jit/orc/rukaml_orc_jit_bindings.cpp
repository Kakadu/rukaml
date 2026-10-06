#include "rukaml_orc_jit.hpp"

#include "llvm/Support/Error.h"
#include "llvm/Support/TargetSelect.h"

#include <string>

extern "C" {
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

static llvm::orc::RukamlJIT *g_jit = nullptr;
static std::string g_err; // глобальный: не имеет деструктора в момент longjmp

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
    // копия нужна: строка OCaml может переместиться при GC
    std::string ir(String_val(ir_module), caml_string_length(ir_module));
    if (llvm::Error err = g_jit->add_ir(ir))
      g_err = llvm::toString(std::move(err));
  } // ir уничтожается здесь, до caml_failwith

  if (!g_err.empty())
    caml_failwith(g_err.c_str());

  CAMLreturn(Val_unit);
}