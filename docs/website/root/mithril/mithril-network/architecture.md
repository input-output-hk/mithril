---
sidebar_position: 1
sidebar_label: Architecture
---

# Mithril network architecture

:::info

* The current Mithril network is based on a single aggregator. However, the team is currently working on a more decentralized version where multiple aggregators will be operating on the same Mithril network.

:::

## Architecture diagram

[![Architecture](images/architecture.svg)](images/architecture.svg)

## Mithril nodes

The network is composed of the following nodes:

* [**Mithril aggregator**](./aggregator.md):

> The trustless node that orchestrates the work of the Mithril signer nodes, gathering their individual signatures to produce Mithril multi-signatures and their associated certificates. The aggregator is also in charge of creating and storing the ledger state snapshot archives.

* [**Mithril signer**](./signer.md):

> The node that works transparently on top of the stake pool operator's Cardano node and individually signs the Cardano chain state.

* [**Mithril client**](./client.md):

> The node used to restore artifacts produced by a Mithril aggregator. It then employs Mithril cryptographic primitives to verify their authenticity and validity.

* **Mithril relay**:

> A forward proxy which is used to secure communications between the Mithril signer and the Mithril aggregator. More information are available at the [Mithril signer deployment model](../../manual/getting-started/run-signer-node#mithril-signer-deployment-model) section

:::tip

For more information about the Mithril protocol, read [about Mithril](../mithril-protocol/protocol.md).

:::