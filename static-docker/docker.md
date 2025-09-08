### Steps to staticly build binaries

prerequisites :

`sudo apt-get install musl-dev musl-tools libssl-dev pkg-config`

⚠️ defaults features have to be removed from mithril-common and mithril-client since we cannot build signer and cli surcharging thoses values
so in the futur we may have to specify manually with wich features we want to build by default inside the command
or find a way to automatise the removing of default features before staticly compile signer and CLI for the docker image 

commands :

```
cargo build --release -p mithril-signer --no-default-features --features bundle_tls,num-integer-backend,jemallocator --target=x86_64-unknown-linux-musl
```
```
cargo build --release -p mithril-client-cli --no-default-features --features bundle_tls,num-integer-backend,enable-http-compression --target=x86_64-unknown-linux-musl
```

docker commands :

`docker build . -t mithril-cardano-bundle`