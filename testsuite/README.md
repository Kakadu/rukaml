# rukaml testsuite

**rukaml testsuite** automatically compiles, builds
and runs rukaml tests according to their specification

The testsuite is structured as follows:
 - [`artifacts`](artifacts) contains promoted build artifacts (object files, executables)
    and generated cram tests
 - [`expected`](expected) contains promoted compiler's output
 - [`tests`](tests) contains compiler's tests


## Quick start
Make sure the environment is configured with `discover` beforehand (see [README](../README.md)).

To run all tests from the testsuite execute one of the following in the project root:
```bash
make testsuite
make testsuite CLEAN=1
make testsuite VERBOSE=1
```

## Test specification
Every test in [`tests`](tests) must be annotated with its specification.

Consider the following test as the simplest example:
```ocaml
(* test (targets llvm rv64 amd64) (run) *)
let main = 0
```
It's specified that the test should be compiled for `llvm`, `rv64`, `amd64` targets
and run on each of them.<br>
Some parameters are omitted in the spec and as such set to their defaults.

Consider the following test with all possible parameters present in the spec:
```ocaml
(*
test
  (src ../abc.ml)
  (targets (anf promote) (rv64 promote) amd64)
  (flags () (--cps --caa))
  (run (exit 2)
    (stdout "first line" "second line"))
*)
```
- `src` - source file that should be used as the compiler's input for the test.<br>
If ommited the file containing the spec is used

- `targets` - targets for the test. All `driver.exe` targerts are supported.<br>
Each target can be annotated with `promote` keyword
which makes the testsuite promote compiler's output to the `expected` directory

- `flags` - sets of flags for the driver.
The test is compiled for each specified target for each specified flag set

- `run` - expected stdout and exit code for the test.<br>
Both `stdout` and `exit` can be omitted and default to `""` and `0` respectively.<br>
If ommited completely the test is not executed but is still compiled and linked


## General pipeline
`make testsuite` runs:

1. `dune build testsuite`<br>
    - Iterates through all the test files in [`tests`](tests) parsing specifications and generating dune rules
    - Executes generated dune rules by building all the tests for all targets
    - Generates cram files for each test with expected stdout and exit code

2. `dune runtest testsuite`<br>
    Runs all generate cram tests asserting stdout and exit code
