

### mithril-end-to-end

Tests args
```bash
Tests args

Usage: mithril-end-to-end [OPTIONS] [COMMAND]

Commands:
  generate-doc  Generate documentation
  help          Print this message or the help of the given subcommand(s)

Options:
      --work-directory <WORK_DIRECTORY>
          A directory where all logs, generated devnet artefacts, snapshots and store folder will be located.
          
          Optional: if not set it will default to `{system_temp_folder}/mithril-end-to-end` Exception for MacOS: default is `./mithril-end-to-end` as the length of the temporary directory's path is too long. It causes the maximum path size of the node.sock file to be exceeded.

      --devnet-scripts-directory <DEVNET_SCRIPTS_DIRECTORY>
          Directory containing scripts to boostrap a devnet
          
          [default: ./devnet]

      --bin-directory <BIN_DIRECTORY>
          Directory to the mithril binaries
          
          It must contains the binaries of the aggregator, signer and client.
          
          Defaults to current folder
          
          [default: .]

      --number-of-bft-nodes <NUMBER_OF_BFT_NODES>
          Number of BFT nodes in the devnet
          
          [default: 1]

      --number-of-pool-nodes <NUMBER_OF_POOL_NODES>
          Number of Pool nodes in the devnet
          
          [default: 2]

      --cardano-slot-length <CARDANO_SLOT_LENGTH>
          Length of a Cardano slot in the devnet (in s)
          
          [default: 0.08]

      --cardano-epoch-length <CARDANO_EPOCH_LENGTH>
          Length of a Cardano epoch in the devnet (in s)
          
          [default: 30]

      --cardano-node-version <CARDANO_NODE_VERSION>
          Cardano node version
          
          [default: 8.7.3]

      --cardano-hard-fork-latest-era-at-epoch <CARDANO_HARD_FORK_LATEST_ERA_AT_EPOCH>
          Epoch at which hard fork to the latest Cardano era will be made (starts with the latest era by default)
          
          [default: 0]

      --mithril-era <MITHRIL_ERA>
          Mithril era to run
          
          [default: thales]

      --mithril-era-reader-adapter <MITHRIL_ERA_READER_ADAPTER>
          Mithril era reader adapter
          
          [default: cardano-chain]

      --signed-entity-types <SIGNED_ENTITY_TYPES>
          Signed entity types parameters (discriminants names in an ordered comma separated list)
          
          [default: CardanoTransactions]

      --run-only
          Enable run only mode

      --use-p2p-network
          Enable P2P network mode

      --skip-cardano-bin-download
          Skip cardano binaries download

  -v, --verbose...
          Verbosity level, add more v to increase

  -h, --help
          Print help (see a summary with '-h')

```
| Subcommand | Aliases | Performed action |
|------------|---------|------------------|
| **generate-doc** | doc | Generate documentation |
| **help** |  | Print this message or the help of the given subcommand(s) |

###  mithril-end-to-end generate-doc

Generate documentation
```bash
Generate documentation

Usage: generate-doc [OPTIONS]

Options:
      --output <OUTPUT>
          Generated documentation file
          
          [default: "[PROGRAM NAME]-command-line.md"]

  -h, --help
          Print help

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `output` | `--output` | - | `OUTPUT` | Generated documentation file | `[PROGRAM NAME]-command-line.md` | - | - |
| `help` | `--help` | `-h` | `HELP` | Print help | - | - | - |
