# Mithril C library Usage

## Running tests

After running `cargo build --release` in the parent directory, build the test executable:

``` sh
$(CXX) test.c -o test -l gtest -lmithril -L ../target/release
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

To test a failing case, compile the program with `FAIL` defined:

``` sh
clang test.c -o test -lmithril -L ../target/release -DFAIL
```

Execute as before:

``` sh
./test "a message to sign"
Not eligible to sign
```
