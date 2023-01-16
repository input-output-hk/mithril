---
sidebar_position: 1
sidebar_label: Welcome
---

# User Manual

Welcome to the **Mithril** user manual!

Let's discover **Mithril in less than 5 minutes**.

## Introduction

A **Mithril Network** is composed of 3 main components:

* **Mithril Aggregator**:

This node is in charge of **coordinating the production of the Cardano snapshot archives** (along with the associated certificates powered by Mithril multi signatures) by interacting with Mithril Signer nodes and a Cardano node.

* **Mithril Signer**:

This node is in charge of **producing single signatures that are then combined into a multi signature** by the Mithril Aggregator. It works side by side with a Cardano node that has stakes in the network (Stake Pool Operator or SPO).

* **Mithril Client**:

This node is in charge of **verifying and restoring a snapshot** that will allow a lightning fast bootstrapping of a Cardano full node.

:::tip

For more information about the **Mithril Protocol**, please refer to the [About Mithril](../mithril/intro.md) section.

:::

## What you'll need

* A Linux (preferred) or a macOS computer.

* A [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (latest stable version).

* A recent version of [Docker Engine](https://docs.docker.com/engine/install/).

* A recent version of [Docker Compose](https://docs.docker.com/compose/install/).

* A recent version of [`jq`](https://stedolan.github.io/jq/) (1.6+).

## What you'll find in the user manual

In the [**Getting Started**](/doc/category/getting-started) guide, you will find tutorials for:

* :new: As someone who needs to [Bootstrap a Cardano node](./getting-started/bootstrap-cardano-node.md) on the `testnet`.

* As an SPO who wants to [Run a Mithril Signer node](./getting-started/run-signer-node.md) on the `testnet`.

* As someone who wants to [Run a Private Mithril network](./getting-started/run-mithril-devnet.md) on the `devnet`.

In the [**Developer Docs**](/doc/category/developer-docs), you will find documentation for:

* The **Mithril Network Nodes**:
  * The [**Mithril Aggregator Node**](./developer-docs/nodes/mithril-aggregator.md) developer documentation.
  * The [**Mithril Signer Node**](./developer-docs/nodes/mithril-signer.md) developer documentation.
  * The [**Mithril Client Node**](./developer-docs/nodes/mithril-client.md) developer documentation.

* The [API Reference](./developer-docs/references.md) guide.

:::tip

If you need help, feel free to reach the **Mithril** team:

* [GitHub Discussions](https://github.com/input-output-hk/mithril/discussions)

* [Stack Exchange](https://cardano.stackexchange.com/questions/tagged/mithril)

:::
