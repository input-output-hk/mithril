---
sidebar_position: 1
---

# Welcome

Mithril is a stake-based multi-signature protocol designed to enhance blockchain efficiency and scalability. By enabling the secure aggregation of cryptographic signatures, it addresses the need for fast synchronization of Cardano nodes and state validation without compromising decentralization or security.

Mithril achieves this by generating snapshots of the blockchain state, which can be certified using aggregated signatures. These signatures rely on a lottery mechanism proportional to stake, ensuring that a quorum of the network participates. This multi-signature process reduces computational overhead, enabling light clients and applications to synchronize quickly and efficiently.

## Who is Mithril for?

Whether you are a stake pool operator (SPO), a full node user, or working with lightweight wallets, Mithril provides tools to streamline operations without compromising security.

**Full node and wallet users.** Mithril’s certified snapshots enable the quick synchronization of a Cardano node. This allows bypassing the time-consuming process of verifying the entire blockchain from scratch, improving the startup experience for full nodes like Daedalus.

**Light clients and decentralized applications (DApps).** Light wallets, mobile apps, and sidechains can utilize Mithril snapshots for secure data synchronization. These snapshots allow clients to operate efficiently with fewer resources while maintaining security assurances.

**Service providers and scaling protocols.** Mithril offers synchronization and interoperability solutions, enabling bridges and scaling protocols to stay updated with the blockchain state securely and with minimal overhead.

**SPOs.** SPOs can participate in creating multi-signatures for blockchain snapshots by running a Mithril signer. This contribution strengthens network security and supports faster node bootstrapping.

Dive into the manual to explore how to:

- [set up](../manual/getting-started/README.mdx)
- [operate](../manual/operate/README.mdx)
- and [develop](../manual/develop/README.mdx) with Mithril.
