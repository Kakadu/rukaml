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
#include "llvm/ExecutionEngine/Orc/SelfExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/Shared/ExecutorSymbolDef.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include <memory>

namespace llvm {
namespace orc {

/// \brief In-process ORC JIT for Rukaml modules.
///
/// RukamlJIT owns the ORC execution session along with the object linking
/// and IR compilation layers that form an in-process JIT. It exposes a
/// minimal interface for adding IR modules to a primary JITDylib and for
/// looking up compiled symbols by name.
class RukamlJIT {
private:
  /// The ORC execution session that owns all JIT state.
  std::unique_ptr<ExecutionSession> _exec_session;

  /// Layer that links relocatable objects into the target process.
  RTDyldObjectLinkingLayer _object_layer;

  /// Layer that compiles LLVM IR into relocatable objects.
  IRCompileLayer _compile_layer;

  /// Data layout describing the target machine.
  DataLayout _data_layout;

  /// Interner used to mangle symbol names for the target.
  MangleAndInterner _mangle;

  /// Thread-safe wrapper around the LLVMContext.
  ThreadSafeContext _ctx;

  /// The primary JITDylib that holds user-added modules.
  JITDylib &_main_jd;

public:
  /// \brief Construct a RukamlJIT from its constituent pieces.
  ///
  /// \param exec_session The execution session that owns the JIT.
  /// \param jtmb         Builder describing the target machine.
  /// \param data_layout  Data layout for the target.
  RukamlJIT(std::unique_ptr<ExecutionSession> exec_session,
            JITTargetMachineBuilder jtmb, DataLayout data_layout);

  ~RukamlJIT();

  /// Парсит текстовое LLVM IR и добавляет модуль в JIT.
  Error add_ir(StringRef ir, ResourceTrackerSP rt = nullptr);

  /// \brief Create a RukamlJIT targeting the host process.
  ///
  /// Builds a SelfExecutorProcessControl, derives the target triple and
  /// default data layout from the host, and constructs a JIT instance.
  ///
  /// \return A newly constructed RukamlJIT, or an Error if the host target
  ///         information could not be obtained.
  static Expected<std::unique_ptr<RukamlJIT>> create();

  /// \brief Get the primary JITDylib.
  ///
  /// \return A reference to the "<main>" JITDylib.
  JITDylib &get_main_jd();

  /// \brief Add an LLVM IR module to the JIT.
  ///
  /// \param tsm The thread-safe module to compile and link.
  /// \param rt  Optional resource tracker. If null, the main JITDylib's
  ///            default resource tracker is used.
  ///
  /// \return An Error indicating success or describing the failure.
  Error ad_module(ThreadSafeModule tsm, ResourceTrackerSP rt = nullptr);

  /// \brief Look up a compiled symbol by name.
  ///
  /// \param name The unmangled symbol name to resolve.
  ///
  /// \return The resolved symbol definition, or an Error if the lookup
  ///         failed.
  Expected<ExecutorSymbolDef> lookup(StringRef name);
};

} // namespace orc
} // namespace llvm

#endif // RUKAML_ORC_JIT_H