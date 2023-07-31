# <p align="center">Mithril :shield:</p>

<div align="center">
  <a href='https://github.com/input-output-hk/mithril/actions'>
    <img src="https://img.shields.io/github/actions/workflow/status/input-output-hk/mithril/ci.yml?label=Tests&style=for-the-badge&branch=main">
  </a>
  <a href='https://github.com/input-output-hk/mithril/issues'>
    <img src="https://img.shields.io/github/issues/input-output-hk/mithril?label=Issues&style=for-the-badge">
  </a>
  <a href='https://github.com/input-output-hk/mithril/network/members'>
     <img src="https://img.shields.io/github/forks/input-output-hk/mithril?label=Forks&style=for-the-badge">
  </a>
  <a href='https://github.com/input-output-hk/mithril/stargazers'>
    <img src="https://img.shields.io/github/stars/input-output-hk/mithril?label=Stars&style=for-the-badge">
  </a>
  <a href='https://github.com/input-output-hk/mithril/blob/main/LICENSE'>
    <img src="https://img.shields.io/github/license/input-output-hk/mithril?label=License&style=for-the-badge">
  </a>
</div>

## :sunny: Introduction

**Mithril** is a research project which goal is to provide [Stake-based Threshold Multisignatures](https://iohk.io/en/research/library/papers/mithril-stake-based-threshold-multisignatures/) on top of the **Cardano network**.

In a nutshell, **Mithril** can be summarized as:

> A protocol that allows **stakeholders** in a **Proof-of-stake (PoS)** blockchain network to individually **sign messages** that are aggregated into a **multi-signature** which guarantees that they represent a minimum share of the total stakes.

In other words, an adversarial participant with less than this share of the total stakes will not be able to produce valid multi-signatures :closed_lock_with_key:.

We have worked on a first implementation of the protocol with the **Mithril network** which goal is to provide a way to fast bootstrap a fully operating **Cardano node**, in less than **2 hours**, whereas it used to take days before.

In order to unleash the power of Mithril, and leverage new use cases, we have also implemented a framework in the **Mithril network** that allows the certification of multiple types of data, provided they can be computed deterministically. 

## :shield: Mainnet availability

> :construction: **Mithril** is currently a work in progress, and is generally available in [**Beta**](https://mithril.network/doc/dev-blog/2023/07/21/mainnet-beta-launch) version.
>
> It is **NOT** yet completely ready for production and **Cardano** mainnet.


## :rocket: Getting started with Mithril

If you are a **Cardano SPO**, a good entrypoint is the [SPO onboarding guide](https://mithril.network/doc/manual/getting-started/SPO-on-boarding-guide). 
Additionally, you can find detailed instructions for running a **Mithril signer** in the [Run a Mithril Signer as an SPO](https://mithril.network/doc/manual/getting-started/run-signer-node) guide.

If you are interested in **fast bootstrapping** a Cardano node, please refer to this [Bootstrap a Cardano node](https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node) guide.

Get access to tutorials, user manual, guides and plenty of documentation on our [website](https://mithril.network/doc)!

Our wiki is also available [here](https://github.com/input-output-hk/mithril/wiki)

## :satellite: Structure of the repository

This repository is sliced in the following parts:

* [**Mithril aggregator**](./mithril-aggregator): the node of the **Mithril network** responsible for collecting individual signatures from the **Mithril signers** and aggregate them into a multi-signature. The **Mithril aggregator** uses this ability to provide certified snapshots of the **Cardano** blockchain.

* [**Mithril client**](./mithril-client): the node of the **Mithril network** responsible for retrieving the certified artifacts produced by the **Mithril network**, e.g. the **Cardano** chain certified snapshots used to securely restore a **Cardano node**.

* [**Mithril common**](./mithril-common): this is the **common** library that is used by the **Mithril network** nodes.

* [**Mithril STM**](./mithril-stm): the **core** library that implements **Mithril** protocol cryptographic engine.

* [**Mithril explorer**](./mithril-explorer): the **explorer** website that connects to a **Mithril aggregator** and displays its **Certificate chain**.

* [**Mithril infrastructure**](./mithril-infra): the infrastructure used to power a **Mithril network** in the cloud.

* [**Mithril signer**](./mithril-signer): the node of the **Mithril network** responsible for producing individual signatures that are collected and aggregated by the **Mithril aggregator**.

* [**Mithril test lab**](./mithril-test-lab): the suite of tools that allow us to test and stress the **Mithril** protocol implementations.

  * [**Mithril devnet**](./mithril-test-lab/mithril-devnet): the private **Mithril/Cardano network** that we use to scaffold a **Mithril network** on top of a **Cardano network**.

  * [**Mithril end to end**](./mithril-test-lab/mithril-end-to-end): the tool that we use to run tests scenari against a **Mithril devnet**.

* [**Protocol demonstration**](./demo/protocol-demo): a simple cli that helps understand how the **Mithril** protocol works and the role of its **protocol parameters**.

## :bridge_at_night: Contributing

The best way to contribute right now is to provide feedback. Start by giving a look at our [documentation](https://mithril.network/doc).

Should you have any questions, ideas or issues, we would like to hear from you:

* #moria on the IOG [Discord server](https://discord.gg/5kaErDKDRq)
* Create a GitHub [Discussion](https://github.com/input-output-hk/mithril/discussions)
* Create a GitHub [Issue](https://github.com/input-output-hk/mithril/issues/new)
* Ask on Cardano [StackExchange](https://cardano.stackexchange.com/questions/tagged/mithril) using the `mithril` tag

When contributing to this project and interacting with others, please follow our [Code of Conduct](./CODE-OF-CONDUCT.md) and our [Contributing Guidelines](./CONTRIBUTING.md).
