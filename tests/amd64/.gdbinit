set logging on
set logging file gdb.log
set disassembly-flavor intel
set history save

tui new-layout example src 1 {-horizontal regs 1 asm 1} 2 status 0 cmd 1
tui layout example
focus cmd

b rukaml_stdlib.c:400
