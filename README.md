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

**Mithril** is a research project the goal of which is to provide [stake-based threshold multi-signatures](https://iohk.io/en/research/library/papers/mithril-stake-based-threshold-multisignatures/)(STM) on top of the **Cardano network**.

In a nutshell, **Mithril** can be summarized as:

> A protocol that enables stakeholders in a proof-of-stake (PoS) blockchain network to individually sign messages, which are then aggregated into a multi-signature, guaranteeing that they represent a minimum share of the total stake.

In other words, an adversarial participant with less than this share of the total stake will be unable to produce valid multi-signatures. :closed_lock_with_key:.

The goal of the first implementation of the Mithril network protocol is to provide a way to quickly bootstrap a fully operating Cardano node in less than two hours, compared to the days it used to take before.

To unleash the power of Mithril and leverage new use cases, we have also implemented a framework in the Mithril network that allows the certification of multiple types of data, provided they can be computed deterministically.

## :shield: Mainnet availability

**Mithril** is currently a work in progress, and is available in its [**beta**](https://mithril.network/doc/dev-blog/2023/07/21/mainnet-beta-launch) version on mainnet.

:heavy_check_mark: It is ready to be safely deployed in the SPO production infrastructure for **Cardano** mainnet.

:warning: It is **NOT** yet completely ready for production usage of the artifacts produced before a minimum level of participation in the network (which depends on the artifact type).

### Disclaimer

By using the Mithril protocol, you understand it is in development and that using the `mithril-signer`, `mithril-aggregator`, and `mithril-client` on mainnet is entirely at your own risk.

You also acknowledge and agree to have an adequate understanding of the risks associated with use of the Mithril network and that all information and materials published, distributed or otherwise made available on the Mithril site and Mithril GitHub repository is available on an ‘AS IS’ and ‘AS AVAILABLE’ basis, without any representations or warranties of any kind. All implied terms are excluded to the fullest extent permitted by law. For details, see also sections 7, 8, and 9 of the [Apache 2.0 License](./LICENSE).

## :rocket: Getting started with Mithril

If you are a **Cardano SPO**, a good entry point is the [SPO onboarding guide](https://mithril.network/doc/manual/getting-started/SPO-on-boarding-guide).
Additionally, you can find detailed instructions for running a **signer node** in [this guide](https://mithril.network/doc/manual/getting-started/run-signer-node).

If you are interested in **fast bootstrapping** of a Cardano node, please refer to [this guide](https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node).

You can access tutorials, the user manual, guides, and plenty of documentation on our [website](https://mithril.network/doc)!

Mithril wiki is also available [here](https://github.com/input-output-hk/mithril/wiki).

## :satellite: Structure of the repository

This repository consists of the following parts:

- [**Mithril aggregator**](./mithril-aggregator): the node of the **Mithril network** responsible for collecting individual signatures from the **Mithril signers** and aggregating them into a multi-signature. The **Mithril aggregator** uses this ability to provide certified snapshots of the **Cardano** blockchain.

- [**Mithril client**](./mithril-client): the **client** library that developers can use to interact with Mithril-certified data in their applications.

- [**Mithril client CLI**](./mithril-client-cli): the CLI used for retrieving the certified artifacts produced by the **Mithril network**, eg, the **Cardano** chain certified snapshots used to securely restore a **Cardano node**.

- [**Mithril client WASM**](./mithril-client-wasm): the WASM-compatible library used for retrieving the certified artifacts produced by the **Mithril network**.

- [**Mithril common**](./mithril-common): the **common** library used by the **Mithril network** nodes.

- [**Mithril STM**](./mithril-stm): the **core** library that implements the **Mithril** protocol cryptographic engine.

- [**Mithril explorer**](./mithril-explorer): the **explorer** website that connects to a **Mithril aggregator** and displays its **certificate chain** and artifacts.

- [**Mithril infrastructure**](./mithril-infra): the infrastructure used to power a **Mithril network** in the cloud.

- [**Mithril signer**](./mithril-signer): the node of the **Mithril network** responsible for producing individual signatures that are collected and aggregated by the **Mithril aggregator**.

- [**Internal**](./internal): the shared tools and API used by **Mithril** crates.

  - [**Mithril build script**](./internal/mithril-build-script): a toolbox for Mithril crates that uses a build script phase.

  - [**Mithril cardano-node-chain**](./internal/cardano-node/mithril-cardano-node-chain): mechanisms to read and interact with the **Cardano chain** through a Cardano node, used by **Mithril network** nodes.

  - [**Mithril cli helper**](./internal/mithril-cli-helper): **CLI** tools for **Mithril** binaries.

  - [**Mithril doc**](./internal/mithril-doc): an API that generates Markdown documentation for crate command line arguments.

  - [**Mithril doc derive**](./internal/mithril-doc-derive): a macro implementation used by **Mithril doc**.

  - [**Mithril era**](./internal/mithril-era): mechanisms to read and check **Mithril Era markers**, used by **Mithril network** nodes.

  - [**Mithril metric**](./internal/mithril-metric): materials to expose **metrics** in **Mithril network** nodes.

  - [**Mithril persistence**](./internal/mithril-persistence): the **persistence** library that is used by **Mithril network** nodes.

  - [**Mithril resource pool**](./internal/mithril-resource-pool): a **resource pool** mechanism that is used by **Mithril network** nodes.

  - [**Mithril ticker**](./internal/mithril-ticker): a **ticker** mechanism that reads time information from the chain and is used by **Mithril network** nodes.

  - [**Mithril signed entity lock**](./internal/signed-entity/mithril-signed-entity-lock): a non-blocking **lock** mechanism for signed entity types, used by **Mithril network** nodes.

  - [**Mithril signed entity prealoader**](./internal/signed-entity/mithril-signed-entity-preloader): a **preload** mechanism for the Cardano transaction signed entity, used by **Mithril network** nodes.

- [**Mithril test lab**](./mithril-test-lab): the suite of tools that allow us to test and stress the **Mithril** protocol implementations.

  - [**Mithril devnet**](./mithril-test-lab/mithril-devnet): the private **Mithril/Cardano network** used to scaffold a **Mithril network** on top of a **Cardano network**.

  - [**Mithril end to end**](./mithril-test-lab/mithril-end-to-end): the tool used to run test scenarios against a **Mithril devnet**.

- [**Protocol demonstration**](./demo/protocol-demo): a simple CLI that helps understand how the **Mithril** protocol works and the role of its **protocol parameters**.

- [**Examples**](./examples): out-of-the-box working examples to get familiar with **Mithril**.

## :bridge_at_night: Contributing

The best way to contribute right now is to provide feedback. Start by looking at our [documentation](https://mithril.network/doc).

Should you have any questions, ideas, or issues, we would like to hear from you:

- #ask-mithril on the IOG [Discord server](https://discord.gg/5kaErDKDRq)
- Create a GitHub [discussion](https://github.com/input-output-hk/mithril/discussions)
- Create a GitHub [issue](https://github.com/input-output-hk/mithril/issues/new)
- Ask on Cardano [StackExchange](https://cardano.stackexchange.com/search?q=mithril) using the `mithril` tag.

When contributing to this project and interacting with others, please follow our [code of conduct](./CODE-OF-CONDUCT.md) and our [contributing guidelines](./CONTRIBUTING.md).

## 🙏 Credits

- Logo created by Alexander Wende
