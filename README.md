# <p align="center">Mithril - Proof of Concept (POC)</p>

<div align="center">
  <a href='https://github.com/input-output-hk/mithril/actions'>
    <img src="https://github.com/input-output-hk/mithril/actions/workflows/ci.yml/badge.svg" />
    <img src="https://img.shields.io/github/workflow/status/input-output-hk/mithril/CI?label=Tests&style=for-the-badge">
    <img src="https://img.shields.io/github/issues/input-output-hk/mithril?label=Issues&style=for-the-badge">
     <img src="https://img.shields.io/github/forks/input-output-hk/mithril?label=Forks&style=for-the-badge">
    <img src="https://img.shields.io/github/stars/input-output-hk/mithril?label=Stars&style=for-the-badge">
    <img src="https://img.shields.io/github/license/input-output-hk/mithril?label=License&style=for-the-badge">
  </a>
</div>

## :sunny: Introduction

**Mithril** is a research project which goal is to provide [Stake-based Threshold Multisignatures](https://iohk.io/en/research/library/papers/mithrilstake-based-threshold-multisignatures/) on top of the **Cardano Network**.

In a nutshell, **Mithril** can be summarized as:

> A protocol that allows **stakeholders** in a **Proof-of-Stake** blockchain network to individually **sign messages** that are aggregated into a **multi signature** which guarantees that they represent a minimum share of the total stakes.

In other words, an adversarial participant with less than this share of the total stakes will not be able to produce valid multi signatures :closed_lock_with_key:.

We have worked on a first implementation of the protocol with the **Mithril Network** which goal is to provide a way to bootstrap fast a fully operating **Cardano Node**, in less than **4 hours** , whereas it used to take days before.

## :construction: Under construction

**Mithril** is currently a work in progress and is still a prototype.

It is **not** yet ready for production and mainnet.

## :satellite: Getting Started with Mithril

:new: Get access to tutorials, user manual, guides and plenty of documentation on our brand [new website](https://mithril.network/doc)!

Our wiki is also available [here](https://github.com/input-output-hk/mithril/wiki)

This repository is sliced in the following parts:

* [**Protocol Demonstration**](./demo/protocol-demo): a simple cli that helps understand how the **Mithril** protocol works and the role of its **protocol parameters**.

* [**Mithril Aggregator**](./mithril-aggregator): the node of the **Mithril Network** responsible for collecting individual signatures from the **Mithril Signers** and aggregate them into a multisignature. The **Mithril Aggregator** uses this ability to provide certified snapshots of the **Cardano** blockchain.

* [**Mithril Client**](./mithril-client): the node of the **Mithril Network** responsible for restoring the **Cardano** blockchain on an empty node from a certified snapshot.

* [**Mithril Common**](./mithril-common): this is the **common** library that is used by the **Mithril Network** nodes.

* [**Mithril Core**](./mithril-core): the **core** library that implements **Mithril** protocol cryptographic engine.

* [**Mithril Infra**](./mithril-infra): the infrastructure used to host a **Mithril Aggregator** in the cloud.

* [**Mithril Signer**](./mithril-signer): the node of the **Mithril Network** responsible for producing individual signatures that are collected and aggregated by the **Mithril Aggregator**.

* [**Mithril Test Lab**](./mithril-test-lab): the suite of tools that allow us to test and stress the **Mithril** protocol implementations.

  * [**Mithril Devnet**](./mithril-test-lab/mithril-devnet): the private **Mithril/Cardano Network** that we use to scaffold a **Mithril Network** on top of a **Cardano Network**.

  * [**Mithril End To End**](./mithril-test-lab/mithril-end-to-end): the tool that we use to run tests scenari against a **Mithril Devnet**.

## :bridge_at_night: Contributing

The best way to contribute right now is to provide feedback. Start by giving a look at our [documentation](https://mithril.network/doc).

Should you have any questions, ideas or issues, we would like to hear from you:

* Create a Github [Discussion](https://github.com/input-output-hk/mithril/discussions)-
* Create a Github [Issue](https://github.com/input-output-hk/mithril/issues/new)
* Ask on Cardano [StackExchange](https://cardano.stackexchange.com/questions/tagged/mithril) using the `mihril` tag

When contributing to this project and interacting with others, please follow our [Code of Conduct](./CODE-OF-CONDUCT.md).
