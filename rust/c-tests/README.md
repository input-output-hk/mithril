# Mithril C library Usage

## Running tests

After running `cargo build --release` in the parent directory, build the test executable:

``` sh
$(CXX) test.c -g -o test -lmithril -lgtest -lgtest_main -L ../target/release
```

To execute the tests:

``` sh
export LD_LIBRARY_PATH=../target/release
./test
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from stm
[ RUN      ] stm.ok
...
```
