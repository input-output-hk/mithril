

### mithril-relay

This program is a relay for Mithril nodes.
```bash
This program is a relay for Mithril nodes.

Usage: mithril-relay [OPTIONS] <COMMAND>

Commands:
  aggregator  Run a relay for a Mithril aggregator
  signer      Run a relay for a Mithril signer
  passive     Run a passive relay (just a peer in the P2P network)
  help        Print this message or the help of the given subcommand(s)

Options:
      --run-mode <RUN_MODE>
          Run Mode
          
          [env: RUN_MODE=]
          [default: dev]

  -v, --verbose...
          Verbosity level (-v=warning, -vv=info, -vvv=debug)

      --config-directory <CONFIG_DIRECTORY>
          Directory where configuration file is located
          
          [default: ./config]

  -h, --help
          Print help

  -V, --version
          Print version

```
| Subcommand | Performed action |
|------------|------------------|
| **aggregator** | Run a relay for a Mithril aggregator |
| **signer** | Run a relay for a Mithril signer |
| **passive** | Run a passive relay (just a peer in the P2P network) |
| **generate-doc** | Generate command line documentation |
| **help** | Print this message or the help of the given subcommand(s) |

The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `run_mode` | `--run-mode` | - | `RUN_MODE` | Run Mode | `dev` | - | - |
| `verbose` | `--verbose` | `-v` | - | Verbosity level (-v=warning, -vv=info, -vvv=debug) | `0` | - | - |
| `config_directory` | `--config-directory` | - | - | Directory where configuration file is located | `./config` | - | - |
| `help` | `--help` | `-h` | - | Print help | - | - | - |
| `version` | `--version` | `-V` | - | Print version | - | - | - |
####  mithril-relay aggregator

Run a relay for a Mithril aggregator
```bash
Run a relay for a Mithril aggregator

Usage: aggregator [OPTIONS] --aggregator-endpoint <AGGREGATOR_ENDPOINT>

Options:
      --listen-port <LISTEN_PORT>
          Peer listening port
          
          [env: LISTEN_PORT=]
          [default: 0]

      --dial-to <DIAL_TO>
          Dial to peer multi-address (e.g. /ip4/0.0.0.0/tcp/1234)
          
          [env: DIAL_TO=]

      --aggregator-endpoint <AGGREGATOR_ENDPOINT>
          Aggregator endpoint URL
          
          [env: AGGREGATOR_ENDPOINT=]

  -h, --help
          Print help

```


| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `listen_port` | `--listen-port` | - | `LISTEN_PORT` | Peer listening port | `0` | - | - |
| `dial_to` | `--dial-to` | - | `DIAL_TO` | Dial to peer multi-address (e.g. /ip4/0.0.0.0/tcp/1234) | - | - | - |
| `aggregator_endpoint` | `--aggregator-endpoint` | - | `AGGREGATOR_ENDPOINT` | Aggregator endpoint URL | - | - | :heavy_check_mark: |
| `help` | `--help` | `-h` | - | Print help | - | - | - |

####  mithril-relay signer

Run a relay for a Mithril signer
```bash
Run a relay for a Mithril signer

Usage: signer [OPTIONS] --aggregator-endpoint <AGGREGATOR_ENDPOINT>

Options:
      --server-port <SERVER_PORT>
          HTTP Server listening port
          
          [env: SERVER_PORT=]
          [default: 3132]

      --listen-port <LISTEN_PORT>
          Peer listening port
          
          [env: LISTEN_PORT=]
          [default: 0]

      --dial-to <DIAL_TO>
          Dial to peer multi-address (e.g. /ip4/0.0.0.0/tcp/1234)
          
          [env: DIAL_TO=]

      --aggregator-endpoint <AGGREGATOR_ENDPOINT>
          Aggregator endpoint URL
          
          [env: AGGREGATOR_ENDPOINT=]

  -h, --help
          Print help

```


| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `server_port` | `--server-port` | - | `SERVER_PORT` | HTTP Server listening port | `3132` | - | - |
| `listen_port` | `--listen-port` | - | `LISTEN_PORT` | Peer listening port | `0` | - | - |
| `dial_to` | `--dial-to` | - | `DIAL_TO` | Dial to peer multi-address (e.g. /ip4/0.0.0.0/tcp/1234) | - | - | - |
| `aggregator_endpoint` | `--aggregator-endpoint` | - | `AGGREGATOR_ENDPOINT` | Aggregator endpoint URL | - | - | :heavy_check_mark: |
| `help` | `--help` | `-h` | - | Print help | - | - | - |

####  mithril-relay passive

Run a passive relay (just a peer in the P2P network)
```bash
Run a passive relay (just a peer in the P2P network)

Usage: passive [OPTIONS]

Options:
      --listen-port <LISTEN_PORT>
          Peer listening port
          
          [env: LISTEN_PORT=]
          [default: 0]

      --dial-to <DIAL_TO>
          Dial to peer multi-address (e.g. /ip4/0.0.0.0/tcp/1234)
          
          [env: DIAL_TO=]

  -h, --help
          Print help

```


| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `listen_port` | `--listen-port` | - | `LISTEN_PORT` | Peer listening port | `0` | - | - |
| `dial_to` | `--dial-to` | - | `DIAL_TO` | Dial to peer multi-address (e.g. /ip4/0.0.0.0/tcp/1234) | - | - | - |
| `help` | `--help` | `-h` | - | Print help | - | - | - |

####  mithril-relay generate-doc

Generate command line documentation
```bash
Generate command line documentation

Usage: generate-doc [OPTIONS]

Options:
      --output <OUTPUT>
          Generated documentation file
          
          [default: "[PROGRAM NAME]-command-line.md"]

  -h, --help
          Print help

```


| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `output` | `--output` | - | - | Generated documentation file | `[PROGRAM NAME]-command-line.md` | - | - |
| `help` | `--help` | `-h` | - | Print help | - | - | - |
