
### mithrildemo

Simple demonstration of the Mithril protocol
```bash
Simple demonstration of the Mithril protocol

Usage: mithrildemo [OPTIONS] [COMMAND]

Commands:
  generate-doc  Generate documentation
  help          Print this message or the help of the given subcommand(s)

Options:
  -m, --m <M>
          Security parameter, upper bound on indices
          
          [default: 200]

  -k, --k <K>
          Quorum parameter
          
          [default: 5]

      --phi-f <PHI_F>
          f in phi(w) = 1 - (1 - f)^w, where w is the stake of a participant
          
          [default: 0.2]

      --nparties <NPARTIES>
          Number of parties
          
          [default: 5]

      --nmessages <NMESSAGES>
          Number of messages to sign
          
          [default: 1]

  -h, --help
          Print help

```
| Subcommand | Aliases | Performed action |
|------------|---------|------------------|
| **generate-doc** | doc | Generate documentation |
| **help** |  | Print this message or the help of the given subcommand(s) |

The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `m` | `--m` | `-m` | `M` | Security parameter, upper bound on indices | `200` | ? | - |
| `k` | `--k` | `-k` | `K` | Quorum parameter | `5` | ? | - |
| `phi_f` | `--phi-f` | - | `PHI_F` | f in phi(w) = 1 - (1 - f)^w, where w is the stake of a participant | `0.2` | ? | - |
| `nparties` | `--nparties` | - | `NPARTIES` | Number of parties | `5` | ? | - |
| `nmessages` | `--nmessages` | - | `NMESSAGES` | Number of messages to sign | `1` | ? | - |
| `help` | `--help` | `-h` | `HELP` | Print help |  | ? | - |
###  mithrildemo generate-doc

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
| `output` | `--output` | - | `OUTPUT` | Generated documentation file | `[PROGRAM NAME]-command-line.md` | ? | - |
| `help` | `--help` | `-h` | `HELP` | Print help |  | ? | - |
