---
sidebar_position: 1
sidebar_label: Welcome
---

# User manual

Welcome to the **Mithril** user manual!

Let's discover **Mithril in less than five minutes**.

## Introduction

A **Mithril network** consists of three main components:

* **Mithril aggregator**:

This node coordinates the production of the Cardano snapshot archives, working alongside Mithril signer and Cardano nodes to generate associated certificates using Mithril multi-signatures.

* **Mithril signer**:

This node is responsible for producing individual signatures, which are then combined into a multi-signature by the Mithril aggregator. It operates in conjunction with a Cardano node (run by a stake pool operator (SPO)) holding stake in the network.

* **Mithril client**:

This node verifies and restores a snapshot, facilitating lightning-fast bootstrapping of a Cardano full node. It plays a crucial role in ensuring the efficiency of the network.

:::tip

For more information about the **Mithril protocol**, see this section [about Mithril](../mithril/intro.md).

:::

## Get started 

To get started with the setup, make sure you have the following components and tools:

* A Linux (preferred) or macOS computer

* A [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (latest stable version)

* A recent version of [Docker Engine](https://docs.docker.com/engine/install/)

* A recent version of [Docker Compose](https://docs.docker.com/compose/install/)

* A recent version of [`jq`](https://stedolan.github.io/jq/) (1.6+)

Having these requirements in place will enable you to proceed smoothly with the setup and configuration for your project.

## Read the user manual 

In the [**Getting started**](/doc/category/getting-started) guide in the user manual, you will find tutorials for:

* [Bootstrapping a Cardano node](./getting-started/bootstrap-cardano-node.md) on `testnet`

* [Running a Mithril signer node as an SPO](./getting-started/run-signer-node.md) on `testnet`

* [Running a private Mithril network](./getting-started/run-mithril-devnet.md) on `devnet`

In the [**Developer docs**](/doc/category/developer-docs), you will find documentation for:

* The **Mithril network nodes**:
  * [**Mithril aggregator node**](./developer-docs/nodes/mithril-aggregator.md) developer documentation
  * [**Mithril signer node**](./developer-docs/nodes/mithril-signer.md) developer documentation
  * [**Mithril client node**](./developer-docs/nodes/mithril-client.md) developer documentation

* The [API reference](./developer-docs/references.md) guide.

:::tip

If you need any assistance, don't hesitate to contact the **Mithril** team: 

* [GitHub discussions](https://github.com/input-output-hk/mithril/discussions)

* [Stack Exchange](https://cardano.stackexchange.com/questions/tagged/mithril)

:::
