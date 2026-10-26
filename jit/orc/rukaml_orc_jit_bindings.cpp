#include "rukaml_orc_jit.hpp"

#include "llvm/Support/Error.h"
#include "llvm/Support/TargetSelect.h"

#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <cstring>
#include <deque>
#include <mutex>
#include <string>
#include <utility>
#include <vector>

extern "C" {
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
}

// Rukaml LLVM runtime (back/llvm/rukaml_stdlib.c) compiled to the same .so
extern "C" {
void myputc(int x);
void *rukaml_apply0(void *f);
void *rukaml_apply1(void *f, void *arg1);
void *rukaml_apply2(void *f, void *arg1, void *arg2);
void *rukaml_apply3(void *f, void *arg1, void *arg2, void *arg3);
void *rukaml_apply4(void *f, void *arg1, void *arg2, void *arg3, void *arg4);
void *rukaml_alloc_pair(void *l, void *r);
void *rukaml_field(int n, void **r);
void *rukaml_alloc_closure(void *func, int32_t argsc);
void *rukaml_applyN(void *f, int32_t argc, ...);
void rukaml_prof_tick(const char *name);
void rukaml_prof_dump(const char *path);
}

namespace {

struct RukamlClosureLayout {
  void *code;
  int32_t argsc;
  int32_t args_received;
  void *args[1];
};

constexpr int kMaxFuncs = 512;
constexpr int kMaxArgs = 4;
constexpr int kMaxCallSites = 512;

enum ProfileState : uint8_t {
  STATE_UNSEEN = 0,
  STATE_CONST = 1,
  STATE_VARIES = 2,
};

struct FuncArgProfile {
  uint8_t state[kMaxArgs];
  int64_t val[kMaxArgs];
};

struct CallSiteProfile {
  uint8_t state;
  uint64_t code_ptr;
  int32_t argsc;
  int32_t args_received;
};

llvm::orc::RukamlJIT *g_jit = nullptr;
std::mutex g_jit_mu;

void **g_slots_base = nullptr;
int64_t *g_cnt_base = nullptr;
int64_t *g_miss_base = nullptr;
int g_num_funcs = 0;

FuncArgProfile g_arg_prof[kMaxFuncs];
CallSiteProfile g_site_prof[kMaxCallSites];

std::mutex g_ev_mu;
std::condition_variable g_ev_cv;
std::deque<int64_t> g_ev_queue;
bool g_ev_queued[kMaxFuncs];
bool g_ev_stop = false;

void reset_online_state() {
  std::lock_guard<std::mutex> lk(g_ev_mu);
  g_slots_base = nullptr;
  g_cnt_base = nullptr;
  g_miss_base = nullptr;
  g_num_funcs = 0;
  std::memset(g_arg_prof, 0, sizeof(g_arg_prof));
  std::memset(g_site_prof, 0, sizeof(g_site_prof));
  g_ev_queue.clear();
  std::memset(g_ev_queued, 0, sizeof(g_ev_queued));
  g_ev_stop = false;
}

} // namespace

extern "C" void rukaml_jit_hot(int64_t func_id) {
  if (func_id < 0 || func_id >= kMaxFuncs)
    return;
  {
    std::lock_guard<std::mutex> lk(g_ev_mu);
    if (g_ev_queued[func_id])
      return;
    g_ev_queued[func_id] = true;
    g_ev_queue.push_back(func_id);
  }
  g_ev_cv.notify_one();
}

extern "C" void rukaml_prof_args(int64_t func_id, int64_t a0, int64_t a1,
                                 int64_t a2, int64_t a3) {
  if (func_id < 0 || func_id >= kMaxFuncs)
    return;
  const int64_t incoming[kMaxArgs] = {a0, a1, a2, a3};
  FuncArgProfile &prof = g_arg_prof[func_id];
  for (int i = 0; i < kMaxArgs; ++i) {
    uint8_t st = __atomic_load_n(&prof.state[i], __ATOMIC_RELAXED);
    if (st == STATE_UNSEEN) {
      prof.val[i] = incoming[i];
      __atomic_store_n(&prof.state[i], STATE_CONST, __ATOMIC_RELEASE);
    } else if (st == STATE_CONST) {
      if (prof.val[i] != incoming[i]) {
        __atomic_store_n(&prof.state[i], STATE_VARIES, __ATOMIC_RELEASE);
      }
    }
  }
}

extern "C" void rukaml_prof_site(int64_t site_id, void *closure_ptr) {
  if (site_id < 0 || site_id >= kMaxCallSites || !closure_ptr)
    return;
  const auto *cl = static_cast<const RukamlClosureLayout *>(closure_ptr);
  uint64_t code = reinterpret_cast<uint64_t>(cl->code);
  int32_t argsc = cl->argsc;
  int32_t recvd = cl->args_received;
  CallSiteProfile &sp = g_site_prof[site_id];
  uint8_t st = __atomic_load_n(&sp.state, __ATOMIC_RELAXED);
  if (st == STATE_UNSEEN) {
    sp.code_ptr = code;
    sp.argsc = argsc;
    sp.args_received = recvd;
    __atomic_store_n(&sp.state, STATE_CONST, __ATOMIC_RELEASE);
  } else if (st == STATE_CONST) {
    if (sp.code_ptr != code || sp.argsc != argsc ||
        sp.args_received != recvd) {
      __atomic_store_n(&sp.state, STATE_VARIES, __ATOMIC_RELEASE);
    }
  }
}

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
    {"rukaml_apply4", addr_of(&rukaml_apply4)},
    {"rukaml_alloc_pair", addr_of(&rukaml_alloc_pair)},
    {"rukaml_field", addr_of(&rukaml_field)},
    {"rukaml_alloc_closure", addr_of(&rukaml_alloc_closure)},
    {"rukaml_applyN", addr_of(&rukaml_applyN)},
    {"rukaml_prof_tick", addr_of(&rukaml_prof_tick)},
    {"rukaml_prof_dump", addr_of(&rukaml_prof_dump)},
    {"rukaml_jit_hot", addr_of(&rukaml_jit_hot)},
    {"rukaml_prof_args", addr_of(&rukaml_prof_args)},
    {"rukaml_prof_site", addr_of(&rukaml_prof_site)},
};

extern "C" CAMLprim value rukaml_orc_create(value unit) {
  CAMLparam1(unit);

  std::string err_msg;
  {
    std::lock_guard<std::mutex> lk(g_jit_mu);
    if (!g_jit) {
      reset_online_state();
      llvm::InitializeNativeTarget();
      llvm::InitializeNativeTargetAsmPrinter();
      llvm::InitializeNativeTargetAsmParser();

      auto jit_or_error = llvm::orc::RukamlJIT::create();
      if (!jit_or_error)
        err_msg = llvm::toString(jit_or_error.takeError());
      else
        g_jit = jit_or_error->release();

      // The runtime is registered exactly once — here, when g_jit is
      // actually created: a second create without it would be a duplicate
      // symbol definition, and registering outside of create belongs to the
      // backend, not the driver.
      if (g_jit) {
        if (llvm::Error err = g_jit->define_abs_symbols(k_runtime_syms)) {
          err_msg = llvm::toString(std::move(err));
          delete g_jit; // let the next create retry everything from scratch
          g_jit = nullptr;
        }
      }
    }
  }

  if (!err_msg.empty())
    caml_failwith(err_msg.c_str());

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value rukaml_orc_update(value ir_module) {
  CAMLparam1(ir_module);

  std::string err_msg;
  {
    std::string ir(String_val(ir_module), caml_string_length(ir_module));
    caml_enter_blocking_section();
    {
      std::lock_guard<std::mutex> lk(g_jit_mu);
      if (!g_jit)
        err_msg = "JIT not created. Call create() first.";
      else if (llvm::Error err = g_jit->add_ir(ir))
        err_msg = llvm::toString(std::move(err));
    }
    caml_leave_blocking_section();
  }

  if (!err_msg.empty())
    caml_failwith(err_msg.c_str());

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value rukaml_orc_add_delta(value ir_module) {
  CAMLparam1(ir_module);

  std::string err_msg;
  {
    std::string ir(String_val(ir_module), caml_string_length(ir_module));
    caml_enter_blocking_section();
    {
      std::lock_guard<std::mutex> lk(g_jit_mu);
      if (!g_jit)
        err_msg = "JIT not created. Call create() first.";
      else if (llvm::Error err = g_jit->add_delta_ir(ir))
        err_msg = llvm::toString(std::move(err));
    }
    caml_leave_blocking_section();
  }

  if (!err_msg.empty())
    caml_failwith(err_msg.c_str());

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value rukaml_orc_run(value name) {
  CAMLparam1(name);

  std::string err_msg;
  uint64_t fn_addr = 0;
  {
    std::string sym(String_val(name), caml_string_length(name));
    caml_enter_blocking_section();
    {
      std::lock_guard<std::mutex> lk(g_jit_mu);
      if (!g_jit) {
        err_msg = "JIT not created. Call create() first.";
      } else {
        auto addr = g_jit->lookup_addr(sym);
        if (!addr)
          err_msg = llvm::toString(addr.takeError());
        else
          fn_addr = *addr;
      }
    }
    caml_leave_blocking_section();
  }

  if (!err_msg.empty())
    caml_failwith(err_msg.c_str());

  int64_t rez = 0;
  caml_enter_blocking_section();
  rez = reinterpret_cast<int64_t (*)()>(static_cast<uintptr_t>(fn_addr))();
  caml_leave_blocking_section();

  CAMLreturn(caml_copy_int64(rez));
}

extern "C" CAMLprim value rukaml_orc_lookup_address(value name) {
  CAMLparam1(name);

  std::string err_msg;
  uint64_t fn_addr = 0;
  {
    std::string sym(String_val(name), caml_string_length(name));
    caml_enter_blocking_section();
    {
      std::lock_guard<std::mutex> lk(g_jit_mu);
      if (!g_jit) {
        err_msg = "JIT not created. Call create() first.";
      } else {
        auto addr = g_jit->lookup_addr(sym);
        if (!addr)
          err_msg = llvm::toString(addr.takeError());
        else
          fn_addr = *addr;
      }
    }
    caml_leave_blocking_section();
  }

  if (!err_msg.empty())
    caml_failwith(err_msg.c_str());

  CAMLreturn(caml_copy_int64(static_cast<int64_t>(fn_addr)));
}

extern "C" CAMLprim value rukaml_orc_bind_tables(value v_num_funcs) {
  CAMLparam1(v_num_funcs);
  int num_funcs = Int_val(v_num_funcs);

  std::string err_msg;
  {
    std::lock_guard<std::mutex> lk(g_jit_mu);
    if (!g_jit) {
      err_msg = "JIT not created. Call create() first.";
    } else {
      reset_online_state();
      g_num_funcs = num_funcs;
      if (num_funcs > 0) {
        auto slots = g_jit->lookup_addr("rukaml.slots");
        auto cnt = g_jit->lookup_addr("rukaml.cnt");
        auto miss = g_jit->lookup_addr("rukaml.miss");
        if (!slots)
          err_msg = llvm::toString(slots.takeError());
        else if (!cnt)
          err_msg = llvm::toString(cnt.takeError());
        else if (!miss)
          err_msg = llvm::toString(miss.takeError());
        else {
          g_slots_base =
              reinterpret_cast<void **>(static_cast<uintptr_t>(*slots));
          g_cnt_base =
              reinterpret_cast<int64_t *>(static_cast<uintptr_t>(*cnt));
          g_miss_base =
              reinterpret_cast<int64_t *>(static_cast<uintptr_t>(*miss));
        }
      }
    }
  }

  if (!err_msg.empty())
    caml_failwith(err_msg.c_str());

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value rukaml_orc_install_slot(value v_slot, value v_sym) {
  CAMLparam2(v_slot, v_sym);
  int slot = Int_val(v_slot);

  std::string err_msg;
  {
    std::string sym(String_val(v_sym), caml_string_length(v_sym));
    caml_enter_blocking_section();
    {
      std::lock_guard<std::mutex> lk(g_jit_mu);
      if (!g_jit) {
        err_msg = "JIT not created. Call create() first.";
      } else if (!g_slots_base || slot < 0 || slot >= g_num_funcs) {
        err_msg = "install_slot: invalid slot index or tables not bound";
      } else {
        auto addr = g_jit->lookup_addr(sym);
        if (!addr) {
          err_msg = llvm::toString(addr.takeError());
        } else {
          void *fn_ptr = reinterpret_cast<void *>(static_cast<uintptr_t>(*addr));
          __atomic_store_n(&g_slots_base[slot], fn_ptr, __ATOMIC_RELEASE);
        }
      }
    }
    caml_leave_blocking_section();
  }

  if (!err_msg.empty())
    caml_failwith(err_msg.c_str());

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value rukaml_orc_next_event(value v_timeout_ms) {
  CAMLparam1(v_timeout_ms);
  int timeout_ms = Int_val(v_timeout_ms);
  int res = -1;

  caml_enter_blocking_section();
  {
    std::unique_lock<std::mutex> lk(g_ev_mu);
    if (g_ev_queue.empty() && !g_ev_stop && timeout_ms > 0) {
      g_ev_cv.wait_for(lk, std::chrono::milliseconds(timeout_ms),
                       [] { return !g_ev_queue.empty() || g_ev_stop; });
    }
    if (!g_ev_queue.empty()) {
      res = static_cast<int>(g_ev_queue.front());
      g_ev_queue.pop_front();
    } else if (g_ev_stop) {
      res = -2;
    } else {
      res = -1;
    }
  }
  caml_leave_blocking_section();

  CAMLreturn(Val_int(res));
}

extern "C" CAMLprim value rukaml_orc_stop_events(value unit) {
  CAMLparam1(unit);
  {
    std::lock_guard<std::mutex> lk(g_ev_mu);
    g_ev_stop = true;
  }
  g_ev_cv.notify_all();
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value rukaml_orc_read_arg_profile(value v_func_id,
                                                      value v_arity) {
  CAMLparam2(v_func_id, v_arity);
  CAMLlocal3(arr, opt, boxed);
  int func_id = Int_val(v_func_id);
  int arity = Int_val(v_arity);
  if (arity < 0)
    arity = 0;
  if (arity > kMaxArgs)
    arity = kMaxArgs;

  arr = caml_alloc(arity, 0);
  for (int i = 0; i < arity; ++i) {
    Store_field(arr, i, Val_none);
  }
  if (func_id >= 0 && func_id < kMaxFuncs) {
    const FuncArgProfile &prof = g_arg_prof[func_id];
    for (int i = 0; i < arity; ++i) {
      uint8_t st = __atomic_load_n(&prof.state[i], __ATOMIC_ACQUIRE);
      if (st == STATE_CONST) {
        boxed = caml_copy_int64(prof.val[i]);
        opt = caml_alloc_some(boxed);
        Store_field(arr, i, opt);
      }
    }
  }
  CAMLreturn(arr);
}

extern "C" CAMLprim value rukaml_orc_read_site_profile(value v_site_id) {
  CAMLparam1(v_site_id);
  CAMLlocal3(tup, opt, boxed_addr);
  int site_id = Int_val(v_site_id);
  if (site_id >= 0 && site_id < kMaxCallSites) {
    const CallSiteProfile &sp = g_site_prof[site_id];
    uint8_t st = __atomic_load_n(&sp.state, __ATOMIC_ACQUIRE);
    if (st == STATE_CONST && sp.code_ptr != 0) {
      boxed_addr = caml_copy_int64(static_cast<int64_t>(sp.code_ptr));
      tup = caml_alloc_tuple(3);
      Store_field(tup, 0, boxed_addr);
      Store_field(tup, 1, Val_int(sp.argsc));
      Store_field(tup, 2, Val_int(sp.args_received));
      opt = caml_alloc_some(tup);
      CAMLreturn(opt);
    }
  }
  CAMLreturn(Val_none);
}

extern "C" CAMLprim value rukaml_orc_read_counts(value v_func_id) {
  CAMLparam1(v_func_id);
  CAMLlocal3(tup, v_cnt, v_miss);
  int func_id = Int_val(v_func_id);
  int64_t cnt = 0;
  int64_t miss = 0;
  if (func_id >= 0 && func_id < g_num_funcs && g_cnt_base && g_miss_base) {
    cnt = __atomic_load_n(&g_cnt_base[func_id], __ATOMIC_RELAXED);
    miss = __atomic_load_n(&g_miss_base[func_id], __ATOMIC_RELAXED);
  }
  v_cnt = caml_copy_int64(cnt);
  v_miss = caml_copy_int64(miss);
  tup = caml_alloc_tuple(2);
  Store_field(tup, 0, v_cnt);
  Store_field(tup, 1, v_miss);
  CAMLreturn(tup);
}

extern "C" CAMLprim value rukaml_orc_reset_func_profile(value v_func_id) {
  CAMLparam1(v_func_id);
  int func_id = Int_val(v_func_id);
  if (func_id >= 0 && func_id < kMaxFuncs) {
    if (func_id < g_num_funcs && g_cnt_base && g_miss_base) {
      __atomic_store_n(&g_cnt_base[func_id], 0, __ATOMIC_RELEASE);
      __atomic_store_n(&g_miss_base[func_id], 0, __ATOMIC_RELEASE);
    }
    std::memset(&g_arg_prof[func_id], 0, sizeof(FuncArgProfile));
    {
      std::lock_guard<std::mutex> lk(g_ev_mu);
      g_ev_queued[func_id] = false;
    }
  }
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value rukaml_orc_destroy(value unit) {
  CAMLparam1(unit);

  {
    std::lock_guard<std::mutex> lk(g_jit_mu);
    reset_online_state();
    // ~RukamlJIT runs endSession(); delete nullptr is safe, so a repeated
    // destroy (and destroy without create) is a no-op.
    delete g_jit;
    g_jit = nullptr;
  }

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value rukaml_orc_runtime_syms(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(res);

  res = Val_emptylist;
  for (auto it = k_runtime_syms.rbegin(); it != k_runtime_syms.rend(); ++it) {
    CAMLlocal1(hd);
    hd = caml_alloc_initialized_string(it->first.size(), it->first.data());
    CAMLlocal1(cell);
    cell = caml_alloc(2, 0);
    Store_field(cell, 0, hd);
    Store_field(cell, 1, res);
    res = cell;
  }

  CAMLreturn(res);
}
