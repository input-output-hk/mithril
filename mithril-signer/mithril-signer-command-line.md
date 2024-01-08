
### mithril-signer

An implementation of a Mithril Signer
```bash
An implementation of a Mithril Signer

Usage: mithril-signer [OPTIONS] [COMMAND]

Commands:
  generate-doc  Generate documentation
  help          Print this message or the help of the given subcommand(s)

Options:
  -r, --run-mode <RUN_MODE>
          Run Mode
          
          [env: RUN_MODE=]
          [default: dev]

  -v, --verbose...
          Verbosity level, add more v to increase

  -c, --configuration-dir <CONFIGURATION_DIR>
          Directory where the configuration file is located
          
          [default: ./config]

      --disable-digests-cache
          Disable immutables digests cache

      --reset-digests-cache
          If set the existing immutables digests cache will be reset.
          
          Will be ignored if set in conjunction with `--disable-digests-cache`.

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version

```
| Subcommand | Aliases | Performed action |
|------------|---------|------------------|
| **generate-doc** | doc | Generate documentation |
| **help** |  | Print this message or the help of the given subcommand(s) |

The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `run_mode` | `--run-mode` | `-r` | `RUN_MODE` | Run Mode | `dev` | ? | - |
| `verbose` | `--verbose` | `-v` |  | Verbosity level, add more v to increase | `0` | ? | - |
| `configuration_dir` | `--configuration-dir` | `-c` |  | Directory where the configuration file is located | `./config` | ? | - |
| `disable_digests_cache` | `--disable-digests-cache` |  |  | Disable immutables digests cache | `false` | ? | - |
| `reset_digests_cache` | `--reset-digests-cache` |  |  | If set the existing immutables digests cache will be reset | `false` | ? | - |
| `help` | `--help` | `-h` |  | Print help (see more with '--help') |  | ? | - |
| `version` | `--version` | `-V` |  | Print version |  | ? | - |

###  mithril-signer generate-doc

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

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `output` | `--output` |  |  | Generated documentation file | `[PROGRAM NAME]-command-line.md` | ? | - |
| `help` | `--help` | `-h` |  | Print help |  | ? | - |

