# Mithril C library Usage

## Environment

Compiling and testing the C library requires a working installation of [clang](https://clang.llvm.org/) and [gtest](https://github.com/google/googletest).
To install those, one can use [nix](https://nixos.org/) and enter `nix-shell` from the top-level directory: This shell comes equiped with the right set of dependencies.
Alternatively, check the documentation of your package-manager for system-dependent install instructions.

Note: For MacOS I made it work by adding `-std=c++<VERSION>` to the `clang` command below, after installing `gtest` as
specified [here](https://github.com/google/googletest/blob/main/googletest/README.md#standalone-cmake-project). 
`<VERSION>` needs to be 11 or higher. 

## Running tests

After running `cargo build --release` in the parent directory, build the test executable:

``` sh
clang -x c++ tests.c stms.c atms.c -g -o tests -L ./target/release -lmithril -lstdc++ -lgtest -lgtest_main
```

**NOTE**: Do not use g++, it does compile but leads to segfault when running the test.

To execute the tests:

``` sh
export LD_LIBRARY_PATH=../target/release
./tests
Running main() from ../googletest/src/gtest_main.cc
[==========] Running 7 tests from 2 test suites.
[----------] Global test environment set-up.
[----------] 5 tests from stm
[ RUN      ] stm.invalidRegistration
[       OK ] stm.invalidRegistration (19 ms)
[ RUN      ] stm.clerkFromPublicData
[       OK ] stm.clerkFromPublicData (86 ms)
[ RUN      ] stm.produceAndVerifyAggregateSignature
[       OK ] stm.produceAndVerifyAggregateSignature (80 ms)
[ RUN      ] stm.failSigningIfIneligible
[       OK ] stm.failSigningIfIneligible (60 ms)
[ RUN      ] stm.dynamicStake
[       OK ] stm.dynamicStake (93 ms)
[----------] 5 tests from stm (340 ms total)

[----------] 2 tests from atm
[ RUN      ] atm.produceAndVerifyAggregateSignature
[       OK ] atm.produceAndVerifyAggregateSignature (45 ms)
[ RUN      ] atm.testingErrors
[       OK ] atm.testingErrors (69 ms)
[----------] 2 tests from atm (115 ms total)

[----------] Global test environment tear-down
[==========] 7 tests from 2 test suites ran. (456 ms total)
[  PASSED  ] 7 tests.
```
