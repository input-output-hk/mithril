# Mithril C library Usage

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
test.c:51: Failure
Expected: (sig[i]) != (nullptr), actual: NULL vs (nullptr)
[  FAILED  ] stm.ok (45 ms)
[----------] 1 test from stm (45 ms total)

[----------] Global test environment tear-down
[==========] 1 test from 1 test suite ran. (45 ms total)
[  PASSED  ] 0 tests.
[  FAILED  ] 1 test, listed below:
[  FAILED  ] stm.ok

 1 FAILED TEST
```
