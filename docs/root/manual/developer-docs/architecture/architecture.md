---
sidebar_position: 1
sidebar_label: Architecture
---

# Mithril Network Architecture

Welcome to the Mithril Network Architecture!

:::tip

For more information about the **Mithril Protocol**, please refer to the [About Mithril](../../../mithril/intro.md) section.

:::

## Introduction

In its current version, the **Mithril Network** is a network of node responsible for creating **Snapshots** and **Certificates** that enable fast bootstrap of a **Cardano Node**. It runs on top of the **Cardano Network**.

:::info

The role of a Mithril Network is to enable end users to restore a full Cardano node in less than 2 hours!

:::

It is basically composed of three nodes:

* [**Mithril Aggregator**](aggregator.md):

> The trust less node that orchestrates the work of the MIthril Signer nodes and that gathers their individual signatures to produce Mithril multi signatures and their associated certificates. It is also in charge of creating and storing the ledger state snapshot archive.

* [**Mithril Signer**](./signer.md):

> The node that works transparently on top of the Stake Pool Operator Cardano nodes and which individually signs the ledger state.

* [**Mithril Client**](./client.md):

> The node used to restore a Cardano full node by retrieving, from a Mithril Aggregator, a remote snapshot, its certificate chain and by verifying their validity thanks to the Mithril cryptographic primitives.

## Architecture Overview

:::info

* This document is subject to change as it is a work in progress.
* We are currenty working on **decentralizing** further the somewhat **centralized** architecture.

:::

[![](images/architecture.jpg)](images/architecture.jpg)
