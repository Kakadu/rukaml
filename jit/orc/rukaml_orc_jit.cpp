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
                    [](const MemoryBuffer &) {
                      return std::make_unique<SectionMemoryManager>();
                    }),
      _compile_layer(*_exec_session, _object_layer,
                     std::make_unique<ConcurrentIRCompiler>(std::move(jtmb))),
      _data_layout(std::move(data_layout)),
      _mangle(*_exec_session, _data_layout),
      _main_jd(_exec_session->createBareJITDylib("<main>")) {
  _main_jd.addGenerator(
      cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
          _data_layout.getGlobalPrefix())));
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

RukamlJIT::~RukamlJIT() {
  if (auto err = _exec_session->endSession())
    _exec_session->reportError(std::move(err));
}

Error RukamlJIT::add_ir(StringRef ir, ResourceTrackerSP rt) {
  SMDiagnostic diag;
  std::unique_ptr<Module> module;
  // ThreadSafeContext::getContext() was removed in LLVM 23: access the
  // context through withContextDo (locks the context mutex for us).
  _ctx.withContextDo(
      [&](LLVMContext *ctx) { module = parseAssemblyString(ir, diag, *ctx); });
  if (!module) {
    std::string msg;
    raw_string_ostream os(msg);
    diag.print("rukaml", os);
    return make_error<StringError>(os.str(), inconvertibleErrorCode());
  }
  return ad_module(ThreadSafeModule(std::move(module), _ctx), std::move(rt));
}

} // namespace orc
} // namespace llvm