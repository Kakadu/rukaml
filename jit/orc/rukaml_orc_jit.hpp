#ifndef RUKAML_ORC_JIT_H
#define RUKAML_ORC_JIT_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/Shared/ExecutorSymbolDef.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include <cstdint>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace llvm {
namespace orc {

/// In-process ORC JIT for Rukaml modules.
class RukamlJIT {
private:
  /// Owns all JIT state.
  std::unique_ptr<ExecutionSession> _exec_session;

  /// Links relocatable objects into the process.
  RTDyldObjectLinkingLayer _object_layer;

  /// Compiles IR into relocatable objects.
  IRCompileLayer _compile_layer;

  /// Target data layout.
  DataLayout _data_layout;

  /// Symbol name mangler.
  MangleAndInterner _mangle;

  /// Primary JITDylib.
  JITDylib &_main_jd;

  /// Resource trackers of delta modules.
  std::vector<ResourceTrackerSP> _version_trackers;

public:
  RukamlJIT(std::unique_ptr<ExecutionSession> exec_session,
            JITTargetMachineBuilder jtmb, DataLayout data_layout);

  ~RukamlJIT();

  /// Parses textual IR and adds it, under [rt] or the default tracker.
  Error add_ir(StringRef ir, ResourceTrackerSP rt = nullptr);

  /// Parses textual IR and adds it under a new per-version tracker.
  Error add_delta_ir(StringRef ir);

  /// Creates a JIT for the host process.
  static Expected<std::unique_ptr<RukamlJIT>> create();

  /// Primary JITDylib.
  JITDylib &get_main_jd();

  /// Adds a module to the JIT.
  Error ad_module(ThreadSafeModule tsm, ResourceTrackerSP rt = nullptr);

  /// Resolves a symbol by name.
  Expected<ExecutorSymbolDef> lookup(StringRef name);

  /// Resolves a symbol's raw address.
  Expected<uint64_t> lookup_addr(StringRef name);

  /// Defines host symbols (name -> address) in the primary JITDylib, e.g. the
  /// rukaml runtime, so that JIT'd code resolves them without -rdynamic.
  Error define_abs_symbols(
      const std::vector<std::pair<std::string, uint64_t>> &syms);
};

} // namespace orc
} // namespace llvm

#endif // RUKAML_ORC_JIT_H
