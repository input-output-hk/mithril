---
sidebar_position: 4
---

import NetworksMatrix from '../../../networks-matrix.md';
import CompiledBinaries from '../../../compiled-binaries.md'

# Mithril client library

:::info

Mithril Client Library can be used by Rust developers to use the Mithril Network in their applications.

:::

:::tip

* For more information about the **Mithril network**, please see the [architecture](../../../mithril/mithril-network/architecture.md) overview.

* For more information about the **Mithril client** node, please see [this overview](../../../mithril/mithril-network/client.md).

* Check out the [`Bootstrap a Cardano node`](../../getting-started/bootstrap-cardano-node.md) guide.

:::

:::note Mithril networks

<NetworksMatrix />

:::

## Resources

| Node | Source repository | Rust documentation | Docker packages |
|:-:|:-----------------:|:------------------:|:---------------:|
**Mithril client** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-client-cli) | [:arrow_upper_right:](https://mithril.network/mithril-client/doc/mithril_client/index.html) | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client)

## Resources

| Node | Source repository | Rust documentation | Docker packages |
|:-:|:-----------------:|:------------------:|:---------------:|
**Mithril client** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-client-cli) | [:arrow_upper_right:](https://mithril.network/mithril-client/doc/mithril_client/index.html) | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client)

## Pre-requisites

* Install the latest stable version of the [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain.

* Install OpenSSL development libraries. For example, on Ubuntu/Debian/Mint, run `apt install libssl-dev`

## Installation

In your project crate, use `cargo` to add [mithril-client](https://crates.io/crates/mithril-client) as a dependency:

```bash
cargo add mithril-client
```

It will add the latest version of the Mithril client library.

Mithril client is an asynchronous library. It has been tested with the crate [tokio](https://crates.io/crates/tokio), In the Cargo.toml of your project, add the following dependency:

```toml
tokio = { version = "1.32.0", features = ["full"] }
``````

## Using Mithril Client Library

If the goal is just to use the existing certificates, it is easier to use the `Client` structure:

```rust
use mithril_client::client::Client;
use mithril_client::common::*;

#[tokio::main]
async fn main() -> StdResult<()> {
    let client = Client::new("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").await?;
    let response = client.list_mithril_stake_distributions().await?;

    for mithril_stake_distribution in response {
        println!("Stake distribution hash = '{}'.", mithril_stake_distribution.hash);
    }

    Ok(())
}
```

Here is an example of the code for the release-preprod network:

```rust
use mithril_client::client::Client;
use mithril_client::common::*;

#[tokio::main]
async fn main() -> StdResult<()> {
    let client = Client::new("https://aggregator.release-mainnet.api.mithril.network/aggregator", "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d").await?;
    let response = client.list_mithril_stake_distributions().await?;

    for mithril_stake_distribution in response {
        println!("Stake distribution hash = '{}'.", mithril_stake_distribution.hash);
    }

    Ok(())
}
```

:::tip

You can read the complete [developer documentation](https://mithril.network/rust-doc/mithril_client/index.html).

:::