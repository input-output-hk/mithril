# Mithril Fake Aggregator

This software is made to test Mithril Client nodes in isolated situations where no real Mithril Aggregator can be reached. It serves a static set of data that can be loaded at start time.

## Fake API

For now, the following routes are implemented:

- GET /aggregator/epoch-settings
- GET /aggregator/certificates
- GET /aggregator/certificate/:hash
- GET /aggregator/artifact/snapshots
- GET /aggregator/artifact/snapshot/:digest
- GET /aggregator/artifact/mithril-stake-distributions/
- GET /aggregator/artifact/mithril-stake-distribution/:hash
- GET /aggregator/status
- GET /aggregator/artifact/cardano-transactions
- GET /aggregator/artifact/cardano-transaction/:hash
- GET /aggregator/artifact/cardano-blocks-transactions
- GET /aggregator/artifact/cardano-blocks-transactions/:hash
- GET /aggregator/artifact/cardano-stake-distributions
- GET /aggregator/artifact/cardano-stake-distribution/:hash
- GET /aggregator/artifact/cardano-stake-distribution/epoch/:epoch
- GET /aggregator/artifact/cardano-database
- GET /aggregator/artifact/cardano-database/epoch/:epoch
- GET /aggregator/artifact/cardano-database/:hash
- GET /aggregator/proof/cardano-transaction
- POST /aggregator/statistics/snapshot

## Data fixtures

It is either possible to use default data provided with the binary or to load a static set of data at start up using the `-d` or `--data-dir` option. When set the given directory is scanned for the following files:

- cardano-blocks-tx-snapshots.json
- cardano-blocks-tx-snapshots-list.json
- cardano-databases.json
- cardano-databases-list.json
- cardano-stake-distributions.json
- cardano-stake-distributions-list.json
- certificates.json
- certificates-list.json
- ctx-proofs.json
- ctx-proofs-list.json
- ctx-snapshots.json
- ctx-snapshots-list.json
- epoch-settings.json
- mithril-stake-distributions.json
- mithril-stake-distributions-list.json
- snapshots.json
- snapshots-list.json
- status.json

For each file, the identifiers of the corresponding artifacts are extracted and the following files are read:

- cardano-blocks-tx-snapshot-{hash}.json
- cardano-database-{hash}.json
- cardano-stake-distribution-{hash}.json
- certificate-{hash}.json
- ctx-proof-{hash}.json
- ctx-snapshot"-{hash}.json
- mithril-stake-distribution-{hash}.json
- snapshot-{digest}.json

If a file is missing or incomplete, the software will stop with an error message.

This project comes with a shell script that reads data from a given Mithril Aggregator URL and creates the data files in a directory:

```
./scripts/import.sh some/data/directory http://valid.mithril.url/aggregator
```

## Process to locally update data

Note: WORKING_DIR_END_TO_END should be short to be used as a socket path (less than 108 charaters).

```
WORKING_DIR_END_TO_END=[SELECT A PATH]
mkdir -p $WORKING_DIR_END_TO_END
./mithril-end-to-end -vvv --work-directory $WORKING_DIR_END_TO_END --bin-directory ../../target/release --devnet-scripts-directory=../mithril-devnet --run-only
```

Waiting some but not too much following lines
`Mithril end to end is running and will remain active until manually stopped...`

In another terminal:

```
WORKING_DIR_END_TO_END=[SELECT A PATH]
JSON_OUTPUT=./default_data
TRANSACTION_HASH_SAMPLE=$(sqlite3 $WORKING_DIR_END_TO_END/stores/aggregator-1/cardano-transaction.sqlite3 "select transaction_hash from cardano_tx")

./scripts/import.sh $JSON_OUTPUT http://localhost:8080/aggregator "$TRANSACTION_HASH_SAMPLE"
```

## Build

Run the following command to build the binary:

```
make build
```

## Command line synopsis

Usage: `mithril-aggregator-fake [OPTIONS]`

Options:

```
  -d, --data-directory <DATA_DIRECTORY>  Directory where the response files are located
  -v, --verbose...                       Verbose mode (-v, -vv, -vvv, etc)
  -q, --quiet                            Quiet mode. Suppress all outputs.
  -p, --tcp-port <TCP_PORT>              TCP port to listen on [default: 80]
  -i, --ip-address <IP_ADDRESS>          IP Address to bind server to [default: 127.0.0.1]
  -h, --help                             Print help
  -V, --version                          Print version
```

## Examples

Launching the fake Aggregator on `127.0.0.1:80` (requires root privileges) using builtin data with the ERROR verbose level:
`./mithril-aggregator-fake`

Launching the fake Aggregator on `127.0.0.1:8000` reading data from `some/data/directory` subdirectory with the INFO verbose level:
`./mithril-aggregator-fake -p 8000 -d some/data/directory -vv`
