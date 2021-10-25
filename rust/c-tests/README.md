# Example usage of C library

After running `cargo build --release` in the parent directory,

``` sh
clang test.c -o test -lmithril -L ../target/release
```

To execute:

``` sh
./test "a message to sign"
Test completed successfully!
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
