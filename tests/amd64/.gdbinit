set logging on
set logging file gdb.log
set disassembly-flavor intel
b main
layout asm
layout reg
r
