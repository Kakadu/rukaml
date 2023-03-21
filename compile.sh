#!/bin/bash
gcc llvm/runtime.c -c -o runtime.o
./_build/default/llvm/llvm_compiler.exe $1 -o result.o -emit-llvm result.ll
gcc runtime.o result.o -o result.exe
rm result.o runtime.o