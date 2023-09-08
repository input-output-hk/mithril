---
sidebar_position: 2
---

# API references

:::info

This page compiles the external developer documentation available for Mithril, intended exclusively for a technical audience.

:::

:::tip

To learn more about the **Mithril protocol**, please refer to the [about Mithril](../../mithril/intro.md) section.

:::

## Mithril networks

import NetworksMatrix from '../../_networks-matrix.md';
<NetworksMatrix />

## A list of dependencies 

| Dependency | Description | Source repository | Rust documentation | REST API
|------------|-------------|:-----------------:|:------------------:|:------------:|
| **Mithril common** | The **common** library used by **Mithril network** nodes. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-common) | [:arrow_upper_right:](https://mithril.network/rust-doc/mithril_common/index.html) | -
| **Mithril STM** | The **core** library that implements the cryptographic engine for the **Mithril** protocol. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-stm) | [:arrow_upper_right:](https://mithril.network/rust-doc/mithril_stm/index.html) | -
| **Mithril aggregator** | The node within the **Mithril network** responsible for collecting individual signatures from the **Mithril signers** and aggregating them into a multi-signature. This capability enables the **Mithril aggregator** to provide certified snapshots of the **Cardano** blockchain. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-aggregator) | [:arrow_upper_right:](https://mithril.network/rust-doc/mithril_aggregator/index.html) | [:arrow_upper_right:](/aggregator-api)
| **Mithril client** | The node within the **Mithril network** responsible for restoring the **Cardano** blockchain on an empty node from a certified snapshot. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-client) | [:arrow_upper_right:](https://mithril.network/rust-doc/mithril_client/index.html) | -
| **Mithril signer** | The node responsible for producing individual signatures that are collected and aggregated by the **Mithril aggregator**. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-signer) | [:arrow_upper_right:](https://mithril.network/rust-doc/mithril_signer/index.html) | -
| **Mithril devnet** | The private **Mithril/Cardano network** used to create a **Mithril network** on top of a private **Cardano network**. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/blob/main/mithril-test-lab/mithril-devnet) | - | -
| **Mithril end to end** | The tool used to run test scenarios against a **Mithril devnet**. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/blob/main/mithril-explorer) | - | -
| **Mithril explorer** | The explorer website that connects to a **Mithril aggregator** and displays its **certificate chain**. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/blob/main/mithril-test-lab/mithril-end-to-end) | - | -
| **Protocol simulation** | A simple CLI that helps understand how the **Mithril protocol** works and the role of its protocol parameters. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/blob/main/demo/protocol-demo) | - | -

