---
sidebar_position: 2
sidebar_label: Why should you use Mithril?
---

# Why should you use Mithril?

Mithril was developed with a focus on optimization, scalability, and interoperability. It addresses several key challenges in the blockchain ecosystem:

## Challenges

- **Slow node bootstrapping.** Synchronizing a full Cardano node from scratch can take days, hindering user participation and development.
- **Light client security and decentralization.** Light clients such as wallets offer efficiency but often rely on centralized and trusted third parties for data, compromising security and decentralization.
- **Layer 2 interaction.** Layer 2 solutions designed to enhance scalability need efficient methods to verify their state and interact with the main chain without requiring validator nodes to run a full Cardano node.

## Solutions

Mithril addresses these challenges through its stake-based threshold multi-signature (STM) scheme. This approach allows for efficient and secure verification of blockchain data without the need to run a full node, thereby improving scalability and accessibility.

Here's how Mithril provides solutions:

- **Fast bootstrapping.** Mithril enables the rapid setup of a full Cardano node in under 20 minutes by providing certified snapshots of the Cardano database. These snapshots can be quickly verified using the multi-signature scheme.
- **Secure and efficient data exchange for lightweight applications.** Lightweight applications such as wallets can leverage Mithril to verify transaction data associated with specific addresses without relying on third parties or running a full node. This maintains decentralization and security while offering greater efficiency.
- **Support for layer 2 solutions.** Mithril allows lightweight verification of the state and transactions of layer 2 solutions, improving their interaction with the main chain. For example, it can verify the stake of validator nodes in a bridge.
