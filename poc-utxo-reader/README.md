# Run the PoC for UTxO reader from immutable files

## Installation
- Install a correctly configured Rust toolchain (latest stable version). You can follow the instructions provided [here](https://www.rust-lang.org/learn/get-started).

- A recent version of [`jq`](https://stedolan.github.io/jq/) (1.6+).

## Build the binary

Switch to the PoC folder:
```bash
$ cd poc-utxo-reader
```

Build the binary with the command:
```bash
$ cargo build --release
$ cp ../target/release/poc-utxo-reader .
```

Verify the binary:
```bash
$ ./poc-utxo-reader --help
This program imports transactions from immutable files and queries the associated UTxOs.

Usage: poc-utxo-reader [OPTIONS] --command <COMMAND>

Options:
      --command <COMMAND>
          Command to run
      --data-directory <DATA_DIRECTORY>
          Data directory where immutable files are located
      --db-path <DB_PATH>
          Database location [default: :memory:]
      --min-immutable-file-number-import <MIN_IMMUTABLE_FILE_NUMBER_IMPORT>
          Min immutable file number imported
      --max-immutable-file-number-import <MAX_IMMUTABLE_FILE_NUMBER_IMPORT>
          Max immutable file number imported
      --with-parallelization
          With parallelization
      --address <ADDRESS>
          Address to lookup records from
      --immutable-file-number-query <IMMUTABLE_FILE_NUMBER_QUERY>
          Immutable file number queried
  -h, --help
          Print help
```

## Import transactions from immutable files

Import all the immutable files in a folder to a custom SQLite store:
```bash
$ ./poc-utxo-reader --command import --db-path ./utxo-preprod.sqlite3 --data-directory ./db/preprod/immutable
```

Import the first 100 immutable files in a folder to a custom SQLite store:
```bash
$ ./poc-utxo-reader --command import --db-path ./utxo-preprod.sqlite3 --data-directory ./db/preprod/immutable --max-immutable-file-number-import 100
```

Import the range [50, 100] of immutable files in a folder to a custom SQLite store:
```bash
$ ./poc-utxo-reader --command import --db-path ./utxo-preprod.sqlite3 --data-directory ./db/preprod/immutable --min-immutable-file-number-import 50 --max-immutable-file-number-import 100
```

Import all the immutable files in a folder to a custom SQLite store with immutable files parallelized scanning (faster, uses more memory; by default import is sequential):
```bash
$ ./poc-utxo-reader --command import --db-path ./utxo-preprod.sqlite3 --data-directory ./db/preprod/immutable --with-parallelization true
```

## Query UTxO from imported immutable files

Query the latest UTxOs for an address:
```bash
$ ./poc-utxo-reader --command query --db-path ./utxo-preprod.sqlite3 --address addr_test1qpkyv2ws0deszm67t840sdnruqgr492n80g3y96xw3p2ksk6suj5musy6w8lsg3yjd09cnpgctc2qh386rtxphxt248qr0npnx | jq .
[
  {
    "address": "addr_test1qpkyv2ws0deszm67t840sdnruqgr492n80g3y96xw3p2ksk6suj5musy6w8lsg3yjd09cnpgctc2qh386rtxphxt248qr0npnx",
    "hash": "b1a7cc8964a6233340e8fb5d4aeb0544f327b15380c3faf2cfa41a166567728a",
    "index": 0,
    "quantity": 2000000,
    "data_hash": "868e5606149c66c3cb8558a098113dffec0758cb26bbe3709f55ec76d4274271"
  },
  {
    "address": "addr_test1qpkyv2ws0deszm67t840sdnruqgr492n80g3y96xw3p2ksk6suj5musy6w8lsg3yjd09cnpgctc2qh386rtxphxt248qr0npnx",
    "hash": "b1a7cc8964a6233340e8fb5d4aeb0544f327b15380c3faf2cfa41a166567728a",
    "index": 1,
    "quantity": 9997822971,
    "data_hash": ""
  }
]
```

Query the UTxOs for an address up to a specific immutable file:
```bash
$ ./poc-utxo-reader --command query --db-path ./utxo-preprod.sqlite3 --address addr_test1qpkyv2ws0deszm67t840sdnruqgr492n80g3y96xw3p2ksk6suj5musy6w8lsg3yjd09cnpgctc2qh386rtxphxt248qr0npnx --immutable-file-number-query 250 | jq .
```

Query the UTxO set for all addresses:
```bash
$ ./poc-utxo-reader --command query --db-path ./utxo-preprod.sqlite3 | jq .
```

Query the UTxO set for all addresses up to a specific immutable file:
```bash
$ ./poc-utxo-reader --command query --db-path ./utxo-preprod.sqlite3 --immutable-file-number-query 250 | jq .
```

