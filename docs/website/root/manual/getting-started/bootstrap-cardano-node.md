---
sidebar_position: 2
---

import NetworksMatrix from '../../networks-matrix.md';
import CompiledBinaries from '../../compiled-binaries.md'

# Bootstrap a Cardano node

:::info

With the **Mithril client** connected to a **Mithril aggregator**, you can restore a full Cardano node on the `mainnet` in less than **20 minutes**!

:::

:::note Mithril networks

<NetworksMatrix />

:::

## Pre-requisites

Before proceeding with the installation, ensure that you have the following pre-requisites in place:

1. **Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain**: make sure you have the latest stable version of Rust installed

2. **Install Build Tools `build-essential` and `m4`**: On Ubuntu/Debian/Mint, run `sudo apt install build-essential m4`.

3. **Install OpenSSL development libraries**: On Ubuntu/Debian/Mint, run the following command to install the required OpenSSL development libraries:

 ```
sudo apt install libssl-dev
 ```

3. **Install other requirements**: Make sure you have all the additional dependencies and requirements specified for the project:

```bash
sudo apt-get install make build-essential m4 docker jq
```

## Download the source file

You can download the source file from GitHub (HTTPS):

```bash
git clone https://github.com/input-output-hk/mithril.git
```

Or (SSH):

```bash
git clone git@github.com:input-output-hk/mithril.git
```

## Build the Mithril client binary

To build the Mithril client binary, switch to the desired branch/tag:

```bash
# Replace **YOUR_BUILD_BRANCH_OR_TAG** with the appropriate branch or tag name
# Please refer to the **Build from** column of the **Mithril networks** table above
git checkout **YOUR_BUILD_BRANCH_OR_TAG**
```

Change the directory:

```bash
cd mithril/mithril-client-cli
```

Run tests (optional):

```bash
make test
```

Build the executable:

```bash
make build
```

## Verify the binary

### Verify the version

To check if the version of the Mithril signer binary is correct, use the following command:

```bash
./mithril-client -V
```

You should see something like:

```bash
mithril-client 0.7.6
```

:warning: Verify that the version displayed corresponds to the version specified in the content of the Release/Pre-release notes (refer to the **Build from** column in the 'Mithril networks' table above).

### Verify the build

To verify that the Mithril client binary is functioning correctly, run the following command to display its help menu:

```bash
./mithril-client -h
```

You should see:

```bash
This program shows, downloads and verifies certified blockchain artifacts.

Usage: mithril-client [OPTIONS] <COMMAND>

Commands:
  snapshot                    Deprecated, use `cardano-db` instead
  cardano-db                  Cardano db management (alias: cdb)
  mithril-stake-distribution  Mithril Stake Distribution management (alias: msd)
  cardano-transaction         [unstable] Cardano transactions management (alias: ctx)
  help                        Print this message or the help of the given subcommand(s)

Options:
      --run-mode <RUN_MODE>
          Run Mode [env: RUN_MODE=] [default: dev]
  -v, --verbose...
          Verbosity level (-v=warning, -vv=info, -vvv=debug)
      --config-directory <CONFIG_DIRECTORY>
          Directory where configuration file is located [default: ./config]
      --aggregator-endpoint <AGGREGATOR_ENDPOINT>
          Override configuration Aggregator endpoint URL [env: AGGREGATOR_ENDPOINT=]
      --log-format-json
          Enable JSON output for logs displayed according to verbosity level
      --log-output <LOG_OUTPUT>
          Redirect the logs to a file
      --unstable
          Enable unstable commands (such as Cardano Transactions)
  -h, --help
          Print help
  -V, --version
          Print version
```

:::tip

To display results in JSON format for the `list` and `show` commands of the Mithril client, you can use the `--json` option:

```bash
./mithril-client cardano-db snapshot list --json
```

:::

:::tip

If you wish to delve deeper and access several levels of logs from the Mithril client, you can use the following:

* Add `-v` for some logs (WARN)
* Add `-vv` for more logs (INFO)
* Add `-vvv` for even more logs (DEBUG)
* Add `-vvvv` for all logs (TRACE)

:::

## Download the pre-built binary

<CompiledBinaries />

## Run the Docker container

The list of available images on the registry is listed [here](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client).

Prepare an environment variable with the selected Docker image:

```bash
export MITHRIL_IMAGE_ID=**YOUR_MITHRIL_IMAGE_ID**
```

Here is an example configuration for the `latest` stable Docker image:

```bash
export MITHRIL_IMAGE_ID=latest
```

Then, create a shell function for the Mithril client:

```bash
mithril_client () {
  docker run --rm -e GENESIS_VERIFICATION_KEY=$GENESIS_VERIFICATION_KEY -e AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT --name='mithril-client' -v $(pwd):/app/data -u $(id -u) ghcr.io/input-output-hk/mithril-client:$MITHRIL_IMAGE_ID $@
}
```

You can now use the `mithril_client` function:

```bash
# 1- Help
mithril_client help

# 2- List snapshots
mithril_client cardano-db snapshot list
```

:::tip

In the following part of the document, you will need to replace the `./mithril-client` commands with `mithril_client` to use the above shell function.

:::

## MSYS2 Windows build

In order to build the `mithril-client` executable on MSYS2 (see
[MSYS2's webpage](https://www.msys2.org/) for installation instructions), the
following packages have to be installed:

```bash
pacman --noconfirm -S base-devel mingw-w64-<env>-x86_64-toolchain mingw-w64-<env>-x86_64-openssl
```

Where `env` has to be replaced with your particular
[environment](https://www.msys2.org/docs/environments/) of choice, for example
`ucrt` if running in the `UCRT64` environment. Use `pacman -Ss <package name>`
to search for packages if unsure.

:::note

If you encounter the following error:
```
    This perl implementation doesn't produce Unix like paths (with forward slash
    directory separators).  Please use an implementation that matches your
    building platform.
```
make sure that the `perl` in `/usr/bin/perl.exe` (from the `perl` package) comes
before the `perl` in `/<env>/bin/perl.exe` (from the
`mingw-w64-<env>-x86_64-perl` package).

:::

From here you have to run the `rustup-init.exe` binary from
[rustup](https://rustup.rs/) and follow along these steps:

```bash
Rust Visual C++ prerequisites

Rust requires a linker and Windows API libraries but they don't seem to be
available.

These components can be acquired through a Visual Studio installer.

1) Quick install via the Visual Studio Community installer
   (free for individuals, academic uses, and open source).

2) Manually install the prerequisites
   (for enterprise and advanced users).

3) Don't install the prerequisites
   (if you're targeting the GNU ABI).

>3


Welcome to Rust!

This will download and install the official compiler for the Rust
programming language, and its package manager, Cargo.

Rustup metadata and toolchains will be installed into the Rustup
home directory, located at:

  C:\Users\<User>\.rustup

This can be modified with the RUSTUP_HOME environment variable.

The Cargo home directory is located at:

  C:\Users\<User>\.cargo

This can be modified with the CARGO_HOME environment variable.

The cargo, rustc, rustup and other commands will be added to
Cargo's bin directory, located at:

  C:\Users\<User>\.cargo\bin

This path will then be added to your PATH environment variable by
modifying the HKEY_CURRENT_USER/Environment/PATH registry key.

You can uninstall at any time with rustup self uninstall and
these changes will be reverted.

Current installation options:


   default host triple: x86_64-pc-windows-msvc
     default toolchain: stable (default)
               profile: default
  modify PATH variable: yes

1) Proceed with standard installation (default - just press enter)
2) Customize installation
3) Cancel installation
>2

I'm going to ask you the value of each of these installation options.
You may simply press the Enter key to leave unchanged.

Default host triple? [x86_64-pc-windows-msvc]
x86_64-pc-windows-gnu

Default toolchain? (stable/beta/nightly/none) [stable]


Profile (which tools and data to install)? (minimal/default/complete) [default]


Modify PATH variable? (Y/n)



Current installation options:


   default host triple: x86_64-pc-windows-gnu
     default toolchain: stable
               profile: default
  modify PATH variable: yes

1) Proceed with selected options (default - just press enter)
2) Customize installation
3) Cancel installation
>1
```

Once the Rust toolchain is installed, one can build the executable normally:

```bash
cd mithril-client-cli
make build
```

This will produce a binary `mithril-client.exe` on the current directory. It can
be invoked the same way as in Linux, with `./mithril-client`.

## Bootstrap a Cardano node from a testnet Mithril Cardano DB snapshot

### Step 1: Prepare some useful variables

```bash
# Cardano network
export CARDANO_NETWORK=**YOUR_CARDANO_NETWORK**

# Aggregator API endpoint URL
export AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT**

# Genesis verification key
export GENESIS_VERIFICATION_KEY=$(wget -q -O - **YOUR_GENESIS_VERIFICATION_KEY**)

# Digest of the latest produced cardano db snapshot for convenience of the demo
# You can also modify this variable and set it to the value of the digest of a snapshot that you can retrieve at step 2
export SNAPSHOT_DIGEST=latest
```

:::tip

In the following commands, we will use the following environment variables:

```bash
# Cardano network
export CARDANO_NETWORK=preview

# Aggregator API endpoint URL
export AGGREGATOR_ENDPOINT=https://aggregator.pre-release-preview.api.mithril.network/aggregator

# Genesis verification key
export GENESIS_VERIFICATION_KEY=$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey)

# Digest of the latest produced cardano db snapshot for convenience of the demo
export SNAPSHOT_DIGEST=latest
```

:::

### Step 2: Select a Cardano DB snapshot

List the available cardano db snapshots with which you can bootstrap a Cardano node:

```bash
./mithril-client cardano-db snapshot list
```

You will see a list of snapshots:

```bash
+-------+-----------+---------+------------------------------------------------------------------+------------+-----------+-----------------------------------+
| Epoch | Immutable | Network | Digest                                                           |       Size | Locations |                           Created |
+-------+-----------+---------+------------------------------------------------------------------+------------+-----------+-----------------------------------+
| 539   | 10787     | preview | db5f50a060d4b813125c4263b700ecc96e5d8c8710f0430e5c80d2f0fa79b667 | 2323485648 | 1         | 2024-04-16 12:56:22.170174972 UTC |
+-------+-----------+---------+------------------------------------------------------------------+------------+-----------+-----------------------------------+
| 539   | 10786     | preview | 6af5dac31e7697c4481426712742f4d6391aea0a5b1df145e08e9eaa105af4a5 | 2323875790 | 1         | 2024-04-16 11:44:25.583804349 UTC |
+-------+-----------+---------+------------------------------------------------------------------+------------+-----------+-----------------------------------+
| 539   | 10785     | preview | 39770647f027a214ac955668dffe4d6d51b9cf67798041de1b003b21ef2208da | 2323295044 | 1         | 2024-04-16 10:31:26.056746652 UTC |
+-------+-----------+---------+------------------------------------------------------------------+------------+-----------+-----------------------------------+
| 539   | 10784     | preview | 9ce64187cb6af25266563e039e8d15962d281482979df94e3ac5c5ca6a914eea | 2323079205 | 1         | 2024-04-16 09:08:14.605224999 UTC |
+-------+-----------+---------+------------------------------------------------------------------+------------+-----------+-----------------------------------+
|       |           |         |                                                                  |            |           |                                   |
…
```

:::warning

If you restore a Cardano node with a version not included in the advertised range of compatible versions, it may cause extra time to restore the node due to ledger computations, or even crash the node.

:::

### Step 3: Show Cardano DB snapshot details

To get more details from a specific snapshot (optional), run:

```bash
./mithril-client cardano-db snapshot show $SNAPSHOT_DIGEST
```

You will see more information about the snapshot:

```bash
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Epoch                 | 539                                                                                                                                                                            |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Immutable File Number | 10787                                                                                                                                                                          |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Network               | preview                                                                                                                                                                        |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Digest                | db5f50a060d4b813125c4263b700ecc96e5d8c8710f0430e5c80d2f0fa79b667                                                                                                               |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Size                  | 2323485648                                                                                                                                                                     |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Cardano node version  | 8.9.0                                                                                                                                                                          |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Location              | https://storage.googleapis.com/cdn.aggregator.testing-preview.api.mithril.network/preview-e539-i10787.db5f50a060d4b813125c4263b700ecc96e5d8c8710f0430e5c80d2f0fa79b667.tar.zst |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Created               | 2024-04-16 12:56:22.170174972 UTC                                                                                                                                              |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Compression Algorithm | Zstandard                                                                                                                                                                      |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
```

### Step 4: Download the selected Cardano DB snapshot

To download the selected snapshot from the remote location to your remote location, run:

```bash
./mithril-client cardano-db download $SNAPSHOT_DIGEST
```


You will see that the selected snapshot archive has been downloaded locally, unpacked, and that the associated certificate is valid:

```bash
1/5 - Checking local disk info…
2/5 - Fetching the certificate and verifying the certificate chain…
3/5 - Downloading and unpacking the cardano db
4/5 - Computing the cardano db message
5/5 - Verifying the cardano db signature…
Cardano db 'db5f50a060d4b813125c4263b700ecc96e5d8c8710f0430e5c80d2f0fa79b667' has been unpacked and successfully checked against Mithril multi-signature contained in the certificate.

    Files in the directory '/home/mithril/data/testnet/db5f50a060d4b813125c4263b700ecc96e5d8c8710f0430e5c80d2f0fa79b667/db' can be used to run a Cardano node with version >= 8.9.0.

    If you are using Cardano Docker image, you can restore a Cardano Node with:

    docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="/home/mithril/data/testnet/db5f50a060d4b813125c4263b700ecc96e5d8c8710f0430e5c80d2f0fa79b667/db",target=/data/db/ -e NETWORK=preview ghcr.io/intersectmbo/cardano-node:8.9.0
```

### Step 5: Launch a Cardano node from the restored Cardano DB snapshot

Launch an empty Cardano node and make it live in minutes!

```bash
docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="$(pwd)/data/testnet/$SNAPSHOT_DIGEST/db",target=/data/db/ -e NETWORK=$CARDANO_NETWORK ghcr.io/intersectmbo/cardano-node:8.9.0
```

You will see the Cardano node start by validating the files ingested from the snapshot archive. Then, it will synchronize with the other network nodes and start adding blocks:

```bash
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:53:40.06 UTC] Chain extended, new tip: 7ae33b2f4bc8b84e77dfd539f0f6e7f59b293e96f62fdcfdb17cbd7a006fe5c0 at slot 63081906
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:55:08.30 UTC] Chain extended, new tip: 6b4ccd2bec5e3862b23ea0f7c2f342a3659cecdcfdaf04551179df3839be6213 at slot 63092090
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:55:21.36 UTC] Chain extended, new tip: 6e95eb82da5a38544e6ef430a2733f6014c3c10527003b9d3bdc534f6a2ce81f at slot 63092103
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:55:39.04 UTC] Chain extended, new tip: a662672ec4b988022e135cb0b7e440f5fbffe8e205771d13a566a418f7021ba7 at slot 63092121
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:55:45.18 UTC] Chain extended, new tip: 2a0f2e6f218a08f4e0bc4668285d8e792fd7ec62f05880bd5b2d23d6bce20dfb at slot 63092127
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:56:18.05 UTC] Chain extended, new tip: ab9ef8af92ec062ec59a10da588e238ba8840705c095ebd5cd5da7ab9ea9c8e1 at slot 63092160
```
