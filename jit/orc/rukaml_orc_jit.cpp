#include "rukaml_orc_jit.hpp"
#include "llvm/AsmParser/Parser.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/SourceMgr.h"

namespace llvm {
namespace orc {

RukamlJIT::RukamlJIT(std::unique_ptr<ExecutionSession> exec_session,
                     JITTargetMachineBuilder jtmb, DataLayout data_layout)
    : _exec_session(std::move(exec_session)),
      _object_layer(*_exec_session,
                    []() { return std::make_unique<SectionMemoryManager>(); }),
      _compile_layer(*_exec_session, _object_layer,
                     std::make_unique<ConcurrentIRCompiler>(std::move(jtmb))),
      _data_layout(std::move(data_layout)),
      _mangle(*_exec_session, _data_layout),
      _ctx(std::make_unique<LLVMContext>()),
      _main_jd(_exec_session->createBareJITDylib("<main>")) {
  // No DynamicLibrarySearchGenerator for the current process — on purpose:
  // the only host code JIT'd code may reference is the rukaml runtime, and it
  // is registered explicitly via define_abs_symbols (see the hpp). A generator
  // would additionally expose the host `main`; for a program without an entry
  // point lookup("main") would silently find and call it.
}

Expected<std::unique_ptr<RukamlJIT>> RukamlJIT::create() {
  auto epc = SelfExecutorProcessControl::Create();
  if (!epc)
    return epc.takeError();

  auto es = std::make_unique<ExecutionSession>(std::move(*epc));

  JITTargetMachineBuilder jtmb(
      es->getExecutorProcessControl().getTargetTriple());

  auto dl = jtmb.getDefaultDataLayoutForTarget();
  if (!dl)
    return dl.takeError();

  return std::make_unique<RukamlJIT>(std::move(es), std::move(jtmb),
                                     std::move(*dl));
}

JITDylib &RukamlJIT::get_main_jd() { return _main_jd; }

Error RukamlJIT::ad_module(ThreadSafeModule tsm, ResourceTrackerSP rt) {
  if (!rt)
    rt = _main_jd.getDefaultResourceTracker();
  return _compile_layer.add(rt, std::move(tsm));
}

Expected<ExecutorSymbolDef> RukamlJIT::lookup(StringRef name) {
  return _exec_session->lookup({&_main_jd}, _mangle(name.str()));
}

Error RukamlJIT::define_abs_symbols(
    const std::vector<std::pair<std::string, uint64_t>> &syms) {
  SymbolMap map;
  for (const auto &s : syms)
    map[_mangle(s.first)] =
        ExecutorSymbolDef(ExecutorAddr(s.second), JITSymbolFlags::Exported);
  return _main_jd.define(absoluteSymbols(std::move(map)));
}

RukamlJIT::~RukamlJIT() {
  if (auto err = _exec_session->endSession())
    _exec_session->reportError(std::move(err));
}

Error RukamlJIT::add_ir(StringRef ir, ResourceTrackerSP rt) {
  SMDiagnostic diag;
  std::unique_ptr<Module> module;
  module = parseAssemblyString(ir, diag, *_ctx.getContext());
  if (!module) {
    std::string msg;
    raw_string_ostream os(msg);
    diag.print("rukaml", os);
    return make_error<StringError>(os.str(), inconvertibleErrorCode());
  }
  // The generated payload carries no datalayout/triple
  // stamp the host's on before handing the module to the compile layer.
  module->setDataLayout(_data_layout);
  module->setTargetTriple(
      _exec_session->getExecutorProcessControl().getTargetTriple().str());
  return ad_module(ThreadSafeModule(std::move(module), _ctx), std::move(rt));
}

} // namespace orc
} // namespace llvm