---
sidebar_position: 2
---

# API Reference

Welcome to the Mithril API references doc!

:::info

This page gathers the external developer documentations available for Mithril. They are intended for a technical audience only.

:::

:::tip

For more information about the **Mithril Protocol**, please refer to the [About Mithril](../../mithril/intro.md) section.

:::

## Dependencies List

| Dependency | Description | Source Repository | Rust Documentation | REST API
|------------|-------------|:-----------------:|:------------------:|:------------:|
| **Mithril Common** | This is the **common** library that is used by the **Mithril Network** nodes. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-common) | [:arrow_upper_right:](https://mithril.network/mithril-common/doc/mithril_common/index.html) | -
| **Mithril Core** | The **core** library that implements **Mithril** protocol cryptographic engine. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-core) | [:arrow_upper_right:](https://mithril.network/mithril-core/doc/mithril/index.html) | -
| **Mithril Aggregator** | The node of the **Mithril Network** responsible for collecting individual signatures from the **Mithril Signers** and aggregate them into a multisignature. The **Mithril Aggregator** uses this ability to provide certified snapshots of the **Cardano** blockchain. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-aggregator) | [:arrow_upper_right:](https://mithril.network/mithril-aggregator/doc/mithril_aggregator/index.html) | [:arrow_upper_right:](/aggregator-api)
| **Mithril Client** | The node of the **Mithril Network** responsible for restoring the **Cardano** blockchain on an empty node from a certified snapshot. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-client) | [:arrow_upper_right:](https://mithril.network/mithril-client/doc/mithril_client/index.html) | -
| **Mithril Signer** | The node of the **Mithril Network** responsible for producing individual signatures that are collected and aggregated by the **Mithril Aggregator**. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-signer) | [:arrow_upper_right:](https://mithril.network/mithril-signer/doc/mithril_signer/index.html) | -
| **Mithril Devnet** | The private **Mithril/Cardano Network** used to scaffold a **Mithril Network** on top of a private **Cardano Network**. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/blob/main/mithril-test-lab/mithril-devnet) | - | -
| **Mithril End to End** | The tool used to run tests scenari against a **Mithril Devnet**. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/blob/main/mithril-explorer) | - | -
| **Mithril Explorer** | The explorer website that connects to a **Mithril Aggregator** and displays its **Certificate Chain**. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/blob/main/mithril-test-lab/mithril-end-to-end) | - | -
| **Protocol Simulation** | A simple cli that helps understand how the **Mithril Protocol** works and the role of its protocol parameters. | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/blob/main/demo/protocol-demo) | - | -

## Mithril Networks

The Mithril Networks are aligned with the Cardano Networks and have the same name

| Network Code | Magic Id | Supported | Aggregator Endpoint | Genesis Verification Key | Note
|------------|:-----------:|:------------:|:-----------------:|:------------------:|:------------|
| `mainnet` | - | :x: | - | - | Not supported yet
| `testnet` | `1097911063` | :x: | - | - | Deprecated, not supported anymore
| `preprod` | `1` | :x: | - | - | Not implemented yet
| `preview` | `2` | :heavy_check_mark: | [:arrow_upper_right:](https://aggregator.api.mithril.network/aggregator) | [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey) | Supported
| `devnet` | `42` | :heavy_check_mark: | [:arrow_upper_right:](http://localhost:8080/aggregator>) | - | Supported on the `devnet` only
