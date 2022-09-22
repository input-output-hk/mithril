# Mithril Aggregator

**This is a work in progress** :hammer_and_wrench:

This is a first version of the Mithril Aggregagator

---

## Pre-requisites

**Install Rust**

- Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (latest stable version).
- Install OpenSSL development libraries, for example on Ubuntu/Debian/Mint run `apt install libssl-dev`
- Ensure `libsqlite3` is installed on your system and check its version is at least `1.35`. Run `apt install libsqlite3` and `sqlite3 --version`


## Mithril test networks

The Mithril test networks are:

- `preview`: Test network with magic id `2`, implemented on the IOG hosted Mitril Aggregator
- `preprod`: Test network with magic id `1`, not implemented yet on the IOG hosted Mithril Aggregator
- `testnet`: Legacy test network with magic id `1097911063`, used to be on the IOG hosted Mitril Aggregator, now deprecated

In this documentation, we use the generic `testnet` identifier, but you need to replace it with the identifier of the network that runs on your Cardano node

## Download source code

```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd mithril-aggregator
```

## Development test and build

```bash
# Test
make test

# Help
make help

# Doc
make doc

# Run in debug with default configuration
make debug
```

## Release build and run binary 'serve' command

Build and run in release with default configuration

```bash
make run
```

Or, build only in release

```bash
make build
```

Display the help menu

```bash
./mithril-aggregator --help
```

You should see

```bash
mithril-aggregator 
Node args

USAGE:
    mithril-aggregator [OPTIONS] <SUBCOMMAND>

OPTIONS:
        --db-directory <DB_DIRECTORY>
            Directory to snapshot [default: /db]

    -h, --help
            Print help information

    -r, --run-mode <RUN_MODE>
            Run Mode [default: dev]

        --server-ip <SERVER_IP>
            Server listening IP [default: 0.0.0.0]

        --server-port <SERVER_PORT>
            Server listening port [default: 8080]

        --snapshot-directory <SNAPSHOT_DIRECTORY>
            Directory to store snapshot Defaults to work folder [default: .]

    -v, --verbose
            Verbosity level

SUBCOMMANDS:
    genesis    Aggregator runs in Genesis tools mode
    help       Print this message or the help of the given subcommand(s)
    serve      Aggregator runs in Serve mode
```

Run 'serve' command in release with default configuration

```bash
./mithril-aggregator serve
```

Run 'serve' command in release with a specific mode

```bash
./mithril-aggregator serve -r testnet
```

Run 'serve' command in release with a custom configuration via env vars

```bash
GENESIS_VERIFICATION_KEY=$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey) RUN_INTERVAL=60000 NETWORK=testnet ./mithril-aggregator serve
```

## Release build and run binary 'genesis' command

Build in release with default configuration

```bash
make build
```

Display the help menu

```bash
./mithril-aggregator genesis --help
```

You should see

```bash
mithril-aggregator-genesis 
Aggregator runs in Genesis tools mode

USAGE:
    mithril-aggregator genesis <SUBCOMMAND>

OPTIONS:
    -h, --help    Print help information

SUBCOMMANDS:
    bootstrap    Bootstrap a genesis certificate Test only usage
    export       Export payload to sign with genesis secret key
    help         Print this message or the help of the given subcommand(s)
    import       Import payload signed with genesis secret key and create & import a genesis certificate
```

Run 'genesis bootstrap' command in release with default configuration, **only in test mode**.
This allows the Mithril Aggregator node to bootstrap a `Genesis Certificate`. After this operation, the Mithril Aggregator will be able to produce new snapshots and certificates.

```bash
./mithril-aggregator genesis bootstrap
```

Or with a specific `Genesis Secret Key`, **only in test mode**.

```bash
./mithril-aggregator genesis bootstrap --genesis-secret-key **YOUR_SECRET_KEY*
```

Run 'genesis export' command in release with default configuration.
This allows the Mithril Aggregator node to export the `Genesis Payload` that needs to be signed (and later reimported) of the `Genesis Certificate`. The signature of the `Genesis Payload` must be done manually with the owner of the `Genesis Secret Key`.

```bash
./mithril-aggregator genesis export
```

Or with a custom export path (to override the default value `./mithril-genesis-payload.txt`)

```bash
./mithril-aggregator genesis export --target-path **YOUR_TARGET_PATH**
```

Run 'genesis import' command in release with default configuration.
This allows the Mithril Aggregator node to import the signed payload of the `Genesis Certificate` and create it in the store. After this operation, the Mithril Aggregator will be able to produce new snapshots and certificates.

```bash
./mithril-aggregator genesis import
```

Or with a custom export path (to override the default value `./mithril-genesis-signed-payload.txt`)

```bash
./mithril-aggregator genesis import --signed-payload-path **YOUR_SIGNED_PAYLOAD_PATH**
```

Run 'genesis import' command in release with a custom configuration via env vars

```bash
GENESIS_VERIFICATION_KEY=$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey) RUN_INTERVAL=60000 NETWORK=testnet ./mithril-aggregator genesis import
```

```

## Build and run Docker container

```bash
# Build Docker image
make docker-build

# Run Docker container
make docker-run
```

## Interact with the Mithril Aggregator

```bash
# Interact with the aggregator through the OpenAPI UI
open -u https://input-output-hk.github.io/mithril/openapi-ui/
```
