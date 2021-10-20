# Example usage of C library

After running `cargo build --release` in the parent directory,

``` sh
clang test.c -o test -I ../include -lmithril -L ../target/release
```

To execute:

``` sh
./test "a message to sign"
```

