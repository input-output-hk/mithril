### Steps to staticly build binaries

questions/problems
- Default values for environmnet variables set for the Cardano node should be extracted from Cardano entrypoint script (or can we run the `printRunEnv` (from the cardano run-mode) function from the entrypoint script without running the whole script ?)
- Do we also support the CLI arguments for the entrypoint? (e.g. `--shelley-kes-key /pool/kes.skey`)
- check signer crash if node socket path is not available yet ? 

prerequisites :

`sudo apt-get install musl-dev musl-tools libssl-dev pkg-config`

⚠️ defaults features have to be removed from mithril-common and mithril-client since we cannot build signer and cli surcharging thoses values
so in the futur we may have to specify manually with wich features we want to build by default inside the command
or find a way to automatise the removing of default features before staticly compile signer and CLI for the docker image 

commands :

`
cargo build --release -p mithril-signer --no-default-features --features bundle_tls,num-integer-backend,jemallocator --target=x86_64-unknown-linux-musl
`

`
cargo build --release -p mithril-client-cli --no-default-features --features bundle_tls,num-integer-backend,enable-http-compression --target=x86_64-unknown-linux-musl
`

### docker commands :

build:

`
docker build . -t mithril-cardano-bundle --progress=plain
`

run :

`
docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data \
    --mount type=bind,source="$(pwd)/db",target=/data/db/ \
    --mount type=bind,source="$(pwd)/mithril",target=/data/mithril/ \
    -e NETWORK=preview \
    -e CARDANO_BLOCK_PRODUCER=true \
    -e CARDANO_SHELLEY_KES_KEY=/todo/kes.sk \
    -e CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE=/todo/opcert.cert \
    -e PARTY_ID=pool15jqsjphnxg7hcx2rvd0ryhg5xwshg7xktthj3zf70nmxx9ffjet \
    -e RUN_INTERVAL=60000 \
    -e STORE_RETENTION_LIMIT=5 \
    mithril-cardano-bundle
`

wait for 100% of replayed blocks.

checking tip of the chain is moving forward:

`
docker exec -it compassionate_ganguly cardano-cli query tip --testnet-magic 2 --socket-path /ipc/node.socket
`
```
{
    "block": 3590825,
    "epoch": 1050,
    "era": "Conway",
    "hash": "3a4e1a81f69c4772a86b1ac643f25cca7a43fdb450a07adff45c864a246546af",
    "slot": 90754380,
    "slotInEpoch": 34380,
    "slotsToEpochEnd": 52020,
    "syncProgress": "100.00"
}
```
connect to docker image through sh and check for versions

`
docker exec -it compassionate_ganguly sh
`

sh-5.2# `/usr/local/bin/mithril-signer --version `

```
mithril-signer 0.2.267
```

sh-5.2# `/usr/local/bin/mithril-client --version`

```
mithril-client 0.12.28
```

settings environement varriables (pre-release-preview) and check we can retrieve latests list of aggregator's snapshots with the client


```sh
export AGGREGATOR_ENDPOINT=https://aggregator.pre-release-preview.api.mithril.network/aggregator
export GENESIS_VERIFICATION_KEY=5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d
export ANCILLARY_VERIFICATION_KEY=5b3138392c3139322c3231362c3135302c3131342c3231362c3233372c3231302c34352c31382c32312c3139362c3230382c3234362c3134362c322c3235322c3234332c3235312c3139372c32382c3135372c3230342c3134352c33302c31342c3232382c3136382c3132392c38332c3133362c33365d
```

sh-5.2# `/usr/local/bin/mithril-client cardano-db snapshot list`

```
Mithril Client CLI version: 0.12.28
+-------+-----------+---------+------------------------------------------------------------------+----------+-----------+-----------------------------------+
| Epoch | Immutable | Network | Digest                                                           |     Size | Locations |                           Created |
+-------+-----------+---------+------------------------------------------------------------------+----------+-----------+-----------------------------------+
| 1050  | 21003     | preview | da5ab152bd41ed75865ccc85d72fdaed32912e5210280581237a2c70f9341336 | 2.42 GiB | 1         | 2025-09-09 08:43:46.183260880 UTC |
+-------+-----------+---------+------------------------------------------------------------------+----------+-----------+-----------------------------------+
| 1050  | 21002     | preview | dceb3eb2523669dc016b28270b08e20c579744572a6bbc961f86b157ae0a1e78 | 2.42 GiB | 1         | 2025-09-09 07:29:39.689047248 UTC |
...
```