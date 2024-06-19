---
sidebar_position: 1
sidebar_label: Introduction
---

# About Mithril

:::info

Explore the Mithril protocol through our [protocol simulation](./mithril-protocol/simulation.md). This interactive experience will provide you with insights into how participants collaborate to generate a **multi-signature**, and you'll also gain a clearer understanding of the protocol parameters.

:::

## Mithril in a nutshell

Mithril is a research project whose goal is to provide [Stake-based Threshold Multisignatures](https://iohk.io/en/research/library/papers/mithrilstake-based-threshold-multisignatures/) on top of the Cardano network.

In a nutshell, Mithril can be summarized as:

> A protocol that allows stakeholders in a proof-of-stake blockchain network to individually sign messages that are aggregated into a multi-signature, which guarantees that they represent a minimum share of the total stake.

In other words, an adversarial participant with less than this share of the total stake will not be able to produce valid multi-signatures :closed_lock_with_key:.

## What you'll find in this guide

In this guide, you will find:

* The **Mithril protocol** documentation:

  * [Mithril protocol in depth](./mithril-protocol/protocol.md)

  * [Mithril certificate chain in depth](./mithril-protocol/certificates.md)

  * An interactive protocol discovery through the [Mithril simulation](./mithril-protocol/simulation.md)

* The **Mithril network** documentation:

  * [Mithril network architecture](./mithril-network/architecture.md)

  * [Mithril aggregator node](./mithril-network/aggregator.md)

  * [Mithril signer node](./mithril-network/signer.md)

  * [Mithril client node](./mithril-network/client.md)

* The [**Mithril threat model**](./threat-model)

:::tip

If you need help, feel free to reach out to the Mithril team:

* [GitHub discussions](https://github.com/input-output-hk/mithril/discussions)

* [Stack Exchange](https://cardano.stackexchange.com/questions/tagged/mithril)

:::
