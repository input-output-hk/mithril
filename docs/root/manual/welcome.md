---
sidebar_position: 1
---

# Welcome

Let's discover **Mithril in less than 5 minutes**.

## Getting Started

A **Mithril Network** is composed of 3 main components:

* **Mithril Aggregator**:

This node is in charge of **coordinating the production of the Cardano snapshot archives** (along with the associated certificates powered by Mithril multi signatures) by interacting with Mithril Signer nodes and a Cardano node.

* **Mithril Signer**:

This node is in charge of **producing single signatures that are then combined into a multi signature** by the Mithril Aggregator. It works side by side with a Cardano node that has stakes in the network (Stake Pool Operator or SPO).

* **Mithril Client**:

This node is in charge of **verifying and restoring a snapshot** that will allow a lightning fast boostrapping of a Cardano full node.

:::tip

For more information about the **Mithril Protocol**, please refer to the [About Mithril](../mithril/intro.md) section.

:::

## What you'll need

* A Linux (preferred) or a macOS computer.

* A [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (version 1.62.0+).

* A recent version of [Docker Engine](https://docs.docker.com/engine/install/).

* A recent version of [Docker Compose](https://docs.docker.com/compose/install/).

* A recent version of [`jq`](https://stedolan.github.io/jq/) (1.6+).

## What you'll find in this guide

In this **Getting Started** guide, you will find tutorials for:

* :new: As someone who needs to [Bootstrap a Cardano node](./getting-started/bootstrap-cardano-node.md) on the `testnet`

* As an SPO who wants to [Run a Mithril Signer node](./getting-started/run-signer-node.md) on the `testnet`

* As someone who wants to [Run a Private Mithril network](./getting-started/run-mithril-devnet.md) on the `devnet`

:::tip

If you need help, feel free to reach the Mithril team:

* [Github Discussions](https://github.com/input-output-hk/mithril/discussions)

* [Stack Exchange](https://cardano.stackexchange.com/questions/tagged/mithril)

:::
