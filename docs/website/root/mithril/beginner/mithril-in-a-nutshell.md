---
sidebar_position: 1
sidebar_label: Mithril in a nutshell
---

# Mithril in a nutshell

Let’s start with some background information to understand the current challenge and the solution that Mithril provides.

Running a Cardano node allows users to interact with the blockchain in a trustless and decentralized way. The network relies on thousands of nodes working together to validate blocks and transactions, creating a unified and secure system. Each node holds a full copy of the blockchain, ensuring independence and decentralization. However, node synchronization is resource-intensive. For example, Daedalus, a full-node wallet, downloads and independently validates every transaction in the blockchain’s history. This process requires significant time, storage, and computational power, creating a barrier for users who lack the necessary resources or technical expertise.

This challenge leaves users and developers with limited choices: invest in the costly setup and maintenance of a full node or rely on centralized services, which compromises Cardano’s core principle of decentralization.

## How Mithril helps

The Mithril protocol is designed to enhance blockchain efficiency and scalability by leveraging stake-based multi-signatures. It achieves higher performance without increasing trust requirements, offering a modular and transparent setup. Signers operate independently to produce individual signatures, which aggregators combine into a single, efficient multi-signature.

Consider this analogy. Think of it like a community vote where only people who own a certain amount of tokens can participate. Each participant’s vote is signed, and these signatures are then combined to show that a significant portion of the token holders agree on the outcome.

Unlike traditional systems requiring all participants to validate data, Mithril uses a stake-based threshold multi-signature scheme (STM). This means only a minimum fraction of the total stake is needed to generate a valid signature, ensuring both security and efficiency.

Operating in a trustless setting, Mithril relies solely on existing proof-of-stake assumptions for consensus security. This makes it ideal for applications such as wallet-as-a-service or mobile clients, which can securely verify and exchange data using certificates from Mithril nodes.

Additionally, Mithril enables rapid Certification by allowing stakeholders to validate checkpoints rather than the entire transaction history. This feature is especially beneficial for light clients, such as light wallets, and extends to use cases like data synchronization between layer 2 chains and full node bootstrapping.
