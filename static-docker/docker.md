### Steps to staticly build binaries

prerequisites :

`sudo apt-get install musl-dev musl-tools libssl-dev pkg-config`

commands (working with previous commit) :

```
cargo build --release -p mithril-client --target=x86_64-unknown-linux-musl

cargo build --release -p mithril-signer --target=x86_64-unknown-linux-musl

cargo build --release -p mithril-client-cli --target=x86_64-unknown-linux-musl
```


commands (with default no modified in cargo.toml files) :

```
cargo build --release -p mithril-common --no-default-features --features num-integer-backend --target=x86_64-unknown-linux-musl
-> ok

cargo build --release -p mithril-client --no-default-features --features rustls-tls,num-integer-backend,enable-http-compression --target=x86_64-unknown-linux-musl
-> ko

cargo build --release -p mithril-client-cli --no-default-features --features bundle_tls --target=x86_64-unknown-linux-musl


cargo build --release -p mithril-signer --no-default-features --features jemallocator,bundle_tls --target=x86_64-unknown-linux-musl
```