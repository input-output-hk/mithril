---
sidebar_position: 4
---

import NetworksMatrix from '../../../networks-matrix.md';
import CompiledBinaries from '../../../compiled-binaries.md'

# Mithril Client Library

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
**Mithril client** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-client) | [:arrow_upper_right:](https://mithril.network/mithril-client/doc/mithril_client/index.html) | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client)

## Resources

| Node | Source repository | Rust documentation | Docker packages |
|:-:|:-----------------:|:------------------:|:---------------:|
**Mithril client** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-client) | [:arrow_upper_right:](https://mithril.network/mithril-client/doc/mithril_client/index.html) | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client)

## Pre-requisites

* Install the latest stable version of the [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain.

* Install OpenSSL development libraries. For example, on Ubuntu/Debian/Mint, run `apt install libssl-dev`

## Installation

In your project crate, use `cargo` to add mithril-client as a dependency:

```bash
cargo add mithril-client
```

It will add the latest version of the Mithril Client Library.

## Using Mithril Client Library

If the goal is just to use the existing certificates, it is easier to use the `Client` structure:

```rust
use mithril-client::client::Client;
use mithril-client::common::*:

#[tokio::main]
async fn main() -> StdResult<()> {
    let client = Client::new("WRITE THE VKEY HERE", &http_server.url()).await?;
    let response = client.list_mithril_stake_distributions().await?;

    for mithril_stake_distribution in response {
        println!("Stake distribution hash = '{}'.", mithril_stake_distribution.hash);
    }

    Ok(())
}
```

You can read the complete [developer documentation](https://mithril.network/rust-doc/mithril_client/index.html).