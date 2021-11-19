# Mithril C library Usage

## Environment

Compiling and testing the C library requires a working installation of [clang](https://clang.llvm.org/) and [gtest](https://github.com/google/googletest).
To install those, one can use [nix](https://nixos.org/) and enter `nix-shell` from the top-level directory: This shell comes equiped with the right set of dependencies.
Alternatively, check the documentation of your package-manager for system-dependent install instructions.

## Running tests

After running `cargo build --release` in the parent directory, build the test executable:

``` sh
clang -x c++ test.c -g -o test -L . -lmithril -lstdc++ -lgtest -lgtest_main
```

**NOTE**: Do not use g++, it does compile but leads to segfault when running the test.

To execute the tests:

``` sh
export LD_LIBRARY_PATH=../target/release
./test
Running main() from ../googletest/src/gtest_main.cc
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from stm
[ RUN      ] stm.ok
[       OK ] stm.ok (55 ms)
[----------] 1 test from stm (55 ms total)

[----------] Global test environment tear-down
[==========] 1 test from 1 test suite ran. (55 ms total)
[  PASSED  ] 1 test.
```
