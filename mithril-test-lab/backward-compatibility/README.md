# Local backward compatibility testing

This tooling reproduces the [CI backward compatibility workflow](../../.github/workflows/backward-compatibility.yml) locally, with the added ability to build custom binaries (e.g. with the `future_snark` Cargo feature) and test them against published releases.

## Prerequisites

- `curl` and `jq` must be installed
- Rust toolchain (for building local binaries)
- A working Cardano devnet environment (the E2E test bootstraps one automatically)

## How it works

The CI backward compatibility strategy tests **one old node among current nodes**. For each `(release-tag, node-type)` combination:

| Scenario       | Aggregator      | Signer          | Client          |
| -------------- | --------------- | --------------- | --------------- |
| Old aggregator | **old release** | release-to-test | release-to-test |
| Old signer     | release-to-test | **old release** | release-to-test |
| Old client     | release-to-test | release-to-test | **old release** |

This tool replicates this strategy locally, using:

- [`mithril-install.sh`](https://raw.githubusercontent.com/input-output-hk/mithril/refs/heads/main/mithril-install.sh) downloaded from the repository to install released binaries (cross-platform: Linux x64/arm64, macOS x64/arm64)
- `cargo build` to create local binaries with custom features
- The `mithril-end-to-end` binary to run E2E tests against a local Cardano devnet

## Quick start

```bash
# 1. Download released binaries (unstable + 3 latest stable releases)
./mithril-backward-compatibility.sh install -d unstable --total-releases 3 --store-directory ./store

# 2. Build local binaries with the future_snark feature
./mithril-backward-compatibility.sh build --label future-snark --features future_snark --build-e2e --store-directory ./store

# 3. Run backward compatibility tests
./mithril-backward-compatibility.sh test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test future-snark --e2e-label future-snark

# 4. Generate a compatibility report
./mithril-backward-compatibility.sh report --store-directory ./store --release-to-test future-snark
```

## Commands

### `install` - Download released binaries

Downloads `mithril-install.sh` from the repository, then uses it to fetch pre-built binaries from GitHub releases and organizes them by version tag.

```bash
# Download specific distributions
./mithril-backward-compatibility.sh install -d unstable -d 2543.0 -d 2524.0 --store-directory ./store

# Download the N most recent stable releases
./mithril-backward-compatibility.sh install --total-releases 5 --store-directory ./store

# Combine both (downloads unstable + 3 latest)
./mithril-backward-compatibility.sh install -d unstable --total-releases 3 --store-directory ./store
```

Binaries are stored in `<store-directory>/<distribution>/`.

### `build` - Build local binaries

Builds binaries from the local workspace with optional Cargo features and stores them under a custom label.

```bash
# Build with default features
./mithril-backward-compatibility.sh build --label local --store-directory ./store

# Build with future_snark feature
./mithril-backward-compatibility.sh build --label future-snark --features future_snark --store-directory ./store

# Also build the E2E runner (needed for testing)
./mithril-backward-compatibility.sh build --label future-snark --features future_snark --build-e2e --store-directory ./store

# Multiple features
./mithril-backward-compatibility.sh build --label custom --features future_snark,bundle_tls --store-directory ./store
```

The built binaries (mithril-aggregator, mithril-signer, mithril-client, mithril-relay) are placed in `<store-directory>/<label>/`. The E2E binary goes to `<store-directory>/e2e-<label>/`.

### `list` - List available binary sets

Shows all installed and built binary sets with their versions.

```bash
./mithril-backward-compatibility.sh list --store-directory ./store
```

### `test` - Run backward compatibility tests

Runs E2E tests by assembling mixed binary sets and executing the `mithril-end-to-end` test runner. Mirrors the CI matrix strategy.

The `--e2e-work-directory` option is required. It specifies where the E2E test runner writes its devnet, cardano nodes, and mithril nodes artifacts during each test run.

```bash
# Test locally built future-snark against auto-detected releases
./mithril-backward-compatibility.sh test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test future-snark --e2e-label future-snark

# Test against specific releases
./mithril-backward-compatibility.sh test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test future-snark --tags 2543.0,2524.0

# Test only specific node types
./mithril-backward-compatibility.sh test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test future-snark --nodes mithril-aggregator

# Custom Cardano node version
./mithril-backward-compatibility.sh test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test unstable --cardano-node-version 10.6.2

# Reduce retry attempts for faster iteration
./mithril-backward-compatibility.sh test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test unstable --max-attempts 1
```

**Exit codes** from each E2E run:

- `0` - success (compatible)
- `2` - retryable error (devnet flake, will retry up to `--max-attempts` times)
- `3` - incompatible (known incompatibility detected by `CompatibilityChecker`)
- other - failure

Results are written as individual JSON files and a `summary.json` in the results directory.

### `report` - Generate compatibility report

Produces a human-readable compatibility matrix from test results.

```bash
# Table format (default, colored terminal output)
./mithril-backward-compatibility.sh report --store-directory ./store --release-to-test future-snark

# Markdown format (same format as the CI workflow summary)
./mithril-backward-compatibility.sh report --store-directory ./store --release-to-test future-snark --format markdown

# JSON format (raw test results)
./mithril-backward-compatibility.sh report --store-directory ./store --format json
```

Example table output:

```
Compatibility        | mithril-signer   | mithril-aggregator   | mithril-client
---------------------+------------------+----------------------+-----------------
2543.0               | OK               | OK                   | OK
2524.0               | OK               | WARN                 | OK
2517.0               | OK               | WARN                 | WARN

Legend: OK = compatible, WARN = incompatible (known), FAIL = test failure
```

Example markdown output:

```markdown
## Distributions backward compatibility

This is the compatibility report of the latest distributions against **'future-snark'**.

**Cardano node version**: 10.6.2

| Compatibility | mithril-signer | mithril-aggregator | mithril-client |
| --- | :---: | :---: | :---: |
| `2543.0` | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| `2524.0` | :heavy_check_mark: | :warning: | :heavy_check_mark: |
| `2517.0` | :heavy_check_mark: | :warning: | :no_entry: |
```

## Directory structure

Binaries and results are stored under `backward-compatibility-binaries/` (configurable with `--store-directory`):

```
backward-compatibility-binaries/
  tags.json                        # List of installed distribution tags
  unstable/                        # Downloaded unstable binaries
    mithril-aggregator
    mithril-signer
    mithril-client
    mithril-relay
  2543.0/                          # Downloaded release binaries
    mithril-aggregator
    mithril-signer
    mithril-client
    mithril-relay
  future-snark/                    # Locally built binaries
    mithril-aggregator
    mithril-signer
    mithril-client
    mithril-relay
  e2e-future-snark/                # E2E runner
    mithril-end-to-end
  results/                         # Test results
    result-tag_2543.0-node_mithril-aggregator.json
    result-tag_2543.0-node_mithril-signer.json
    summary.json
```

The E2E work directory (specified with `--e2e-work-directory`) is separate and contains ephemeral artifacts from test runs:

```
/tmp/backward-compat-work/         # E2E work directory (user-specified)
  e2e-run-2543.0-mithril-aggregator/
    binaries/                      # Assembled mixed binary set
    artifacts/                     # Devnet and node artifacts
  e2e-run-2543.0-mithril-signer/
    ...
```

## Common scenarios

### Testing a feature branch against stable releases

```bash
# Download the 3 latest stable releases
./mithril-backward-compatibility.sh install --total-releases 3 --store-directory ./store

# Build your branch with custom features and the E2E runner
./mithril-backward-compatibility.sh build --label future_snark --features future_snark --build-e2e --store-directory ./store

# Run the full compatibility matrix
./mithril-backward-compatibility.sh test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test future_snark --e2e-label future_snark

# View the report
./mithril-backward-compatibility.sh report --store-directory ./store --release-to-test future_snark
```

### Reproducing the exact CI workflow locally

```bash
# Download unstable + 3 latest releases (same as CI defaults)
./mithril-backward-compatibility.sh install -d unstable --total-releases 3 --store-directory ./store

# Build the E2E runner from your workspace
./mithril-backward-compatibility.sh build --label unstable-e2e --build-e2e --store-directory ./store

# Run tests (unstable vs latest 3 releases)
./mithril-backward-compatibility.sh test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test unstable --e2e-label unstable-e2e

# Generate report
./mithril-backward-compatibility.sh report --store-directory ./store --release-to-test unstable
```

### Testing only the aggregator

```bash
./mithril-backward-compatibility.sh test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test future-snark --nodes mithril-aggregator --max-attempts 1
```

## Differences from CI

| Aspect            | CI                                     | Local tool                                        |
| ----------------- | -------------------------------------- | ------------------------------------------------- |
| Binary download   | `gh release download` (linux-x64 only) | `mithril-install.sh` downloaded from repository (Linux + macOS, x64 + arm64) |
| Custom features   | Not supported                          | `--features` flag on build                        |
| E2E runner source | Built from a specific git ref          | Built from current workspace                      |
| Retry mechanism   | `nick-fields/retry@v3`                 | Built-in retry loop                               |
| Result artifacts  | GitHub Actions artifacts               | Local JSON files                                  |
| Report format     | GitHub Actions step summary (Markdown) | Terminal table or JSON                            |
