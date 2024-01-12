# Mithril Fake Aggregator

This software is made to test Mithril Client nodes in isolated situations where no real Mithril Aggregator can be reached. It serves a static set of data that can be loaded at start time.

## Fake API

For now, the following routes are implemented:

 * GET /aggregator/epoch-settings
 * GET /aggregator/certificates
 * GET /aggregator/certificate/:hash
 * GET /aggregator/artifact/snapshots
 * GET /aggregator/artifact/snapshot/:digest
 * GET /aggregator/artifact/mithril-stake-distributions/
 * GET /aggregator/artifact/mithril-stake-distribution/:hash

## Data fixtures

It is either possible to use default data provided with the binary or to load a static set of data at start up using the `-d` or `--data-dir` option. When set the given directory is scanned for the following files:

 * certificates.json
 * snapshots.json
 * mithril-stake-distributions.json

For each file, the identifiers of the corresponding artifacts are extracted and the following files are read:

 * certificate-{hash}.json
 * snapshot-{digest}.json
 * mithril-stake-distribution-{hash}.json

If a file is missing or incomplete, the software will stop with an error message.

## Command line synopsis

Usage: `mithril-fake-aggregator [OPTIONS]`

Options:

```
  -d, --data-directory <DATA_DIRECTORY>  Directory where the response files are located
  -v, --verbose...                       Verbose mode (-q, -v, -vv, -vvv, etc)
  -p, --tcp-port <TCP_PORT>              TCP port to listen on [default: 80]
  -i, --ip-address <IP_ADDRESS>          IP Address to bind server to [default: 127.0.0.1]
  -h, --help                             Print help
  -V, --version                          Print version
```
