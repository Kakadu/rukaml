# rukaml

[![Build](https://github.com/Kakadu/rukaml/actions/workflows/master.yml/badge.svg)](https://github.com/Kakadu/rukaml/actions/workflows/master.yml)
[![License](https://img.shields.io/badge/license-LGPL-blue)](https://github.com/Kakadu/miniml/blob/master/LICENSE.md)
[![API docs](https://img.shields.io/badge/API-documentation-yellowgreen)](https://kakadu.github.io/miniml/api)

(mini)ML compiler 💡


## Features
Take a look at the [testsuite](testsuite/tests) to get a grasp of what's supported

The following features are implemented:
- HM Type checker
- functions
- tuples
- runtime primitives (e.g. `print`)
- simple copying GC

Backends:
- RISC V (the most mature one)
- AMD64
- LLVM


## Getting started
Install dependencies using `opam`:
```fish
opam install --deps-only --with-doc --with-dev-setup .
```

Build the project and run some tests:
```fish
dune build
dune runtest
```

### `discover` the environment and run the testsuite
On the first run of `dune build` rukaml `discover`s your environment:
```
discover: discovering AMD64 compiler
discover: "x86_64-unknown-linux-gnu-gcc" is not available
discover: ((compile ()) (assemble ()) (link ()) (run ()))

discover: discovering RV64 compiler
discover: discovering RV64 assembler
discover: discovering RV64 linker
discover: discovering RV64 runner
discover: ((compile ("riscv64-unknown-linux-gnu-gcc -g -fPIC -Wall -Wpedantic"))
          (assemble ("riscv64-unknown-linux-gnu-gcc -x assembler -c"))
          (link ("riscv64-unknown-linux-gnu-gcc "))
          (run ("qemu-riscv64 -L /opt/rv64/sysroot")))
```

If some target's toolchain is not found tests in the testsuite for this target are **automatically skipped**.

To configure `discover` use environment variables.<br>
E.g. consider the following [direnv](https://github.com/direnv/direnv) `.envrc`
to compile and run `amd64` tests natively:
```fish
export CC_AMD64=gcc
export LD_AMD64=gcc
export RUN_AMD64=""
export RUN_FLAGS_AMD64=""

```

Once the environment is properly discovered run the [testsuite](testsuite):
```fish
make testsuite
```

For more information on the testsuite see its [README](testsuite/README.md)
