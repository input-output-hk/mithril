---
sidebar_position: 3
sidebar_label: How it works
---

# How it works

The protocol utilizes a lottery mechanism where the probability of a signer winning is directly proportional to their stake. This lottery determines which signers are eligible to participate in the signing round. The generated signatures are then combined by an aggregator into a **Mithril multi-signature**, which requires a predefined threshold (quorum) of the total stake to be reached.

The **Mithril protocol** consists of three phases:

- **Protocol establishment.** During this phase, the protocol parameters are established. These parameters are crucial for security and include:
  - **m**: the number of lotteries each signer can participate in for each message
  - **k**: the minimum number of unique winning lottery indices needed to create a multi-signature
  - **f**: a tuning parameter that adjusts the chances of signers winning a lottery based on their stake.
- **Initialization.** In this phase, participating signers generate and broadcast their keys along with a proof of possession. This process happens at the start of each new epoch, which is five days on Cardano. The aggregate verification key (AVK), a condensed representation of the key registration process, is computed and later used to sign and verify multi-signatures.
- **Operations.** This phase involves the creation and broadcasting of individual signatures by signers. Aggregators collect these signatures and combine them into multi-signatures once the quorum is reached. These multi-signatures are then sealed into a Mithril certificate along with the AVK.

:::info

Refer to the [Protocol phases](../advanced/mithril-protocol/protocol.md) overview for more details.

:::

Mithril involves three main participants – **signers, aggregators, and clients** – working together to ensure secure and efficient data verification.

The operations phase is the core of the Mithril process, involving the generation of individual signatures, their aggregation into a multi-signature, and the creation of a certificate that verifies the authenticity of blockchain data:

- **Message signing.** When a new message, such as a snapshot of the Cardano blockchain, requires certification, a multi-party signing round is initiated.
- **Lottery participation.** Each signer (SPO) participates in the lottery process pertaining to the message, with the likelihood of winning influenced by the signer's stake and other parameters.
- **Individual signature generation.** Signers who win at least one lottery create individual signatures for the message using their specific signing key, sending these signatures to the Mithril aggregator.
- **Aggregation.** The aggregator collects individual signatures and combines them into a single multi-signature once the requisite number of unique winning lottery indices is met (quorum).
- **Certificate generation.** This multi-signature, along with the signed message and additional metadata, forms a Mithril certificate. This certificate serves as verification that a sufficient amount of stake has endorsed the message.
- **Verification.** Clients, including light wallets or node operators, can confirm the authenticity of their assets (information retrieved from some untrusted sources) via the certificates. The verification process is efficient and does not require downloading the entire blockchain history; it entails checking the multi-signature against the known aggregate verification key and tracing the certificate back to a trusted genesis certificate.

To fully understand Mithril, it is essential to explore its advanced operational aspects, which include its cryptographic foundations, the roles and security requirements of SPOs, and the strategic upgrade paths for the network.

To explore advanced topics, see:

- Mithril protocol
  - [Protocol phases](../advanced/mithril-protocol/protocol.md)
  - [Certificate chain design](../advanced/mithril-protocol/certificates.md)
  - [Protocol security](../advanced/mithril-protocol/security.md)
  - [Threat model analysis](../advanced/threat-model.md).
- Mithril network
  - [Architecture](../advanced/mithril-network/architecture.md)
  - [Mithril aggregator](../advanced/mithril-network/aggregator.md)
  - [Mithril signer](../advanced/mithril-network/signer.md)
  - [Mithril client](../advanced/mithril-network/client.md).
