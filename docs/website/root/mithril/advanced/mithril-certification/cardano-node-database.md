---
sidebar_position: 3
sidebar_label: Cardano node database
---

# Cardano node database

## Introduction

The Mithril protocol supports the certification of the **Cardano node internal database**. This allows users to fast bootstrap a Cardano node.
This is particularly useful for full-node wallets, SPOs and layer-2 protocols that need to operate full Cardano nodes.

To do so, the Mithril signers and aggregators independently compute a message which represents the imutable files of the **Cardano node internal database** and apply the Mithril protocol to jointly sign it.

:::info

It is worth mentioning that the Cardano node internal database:

- weighs more than `150 GB` on the Cardano mainnet
- is fast bootstrapped in `~20 min` with Mithril
- is bootstrapped in `>24 h` without Mithril.

:::

## Mithril certification

The Mithril certification is generally done with the following steps:

- the **signers** compute the message which represents the type of information to sign, and then broadcast it to the aggregators
- the **aggregators** compute the same message which represents the type of information to sign, collect the signatures from the signers and attempt to aggregate them into a multi-signature
- the **clients** download "artifacts" from an untrusted source (e.g. an aggregator, a cloud service, or a peer-to-peer network), re-compute the same message which represents the type of information to verify, and verify that the message is signed by a valid Mithril multi-signature.

[![Design of the certification of the Cardano node internal database](./images/cardano-node-database/end-to-end-process.jpg)](./images/cardano-node-database/end-to-end-process.jpg)
<small><center>End to end certification for Cardano transactions</center></small>

### Message computation

The message is the hash of the concatenation of the immutable files in the Cardano node internal database:

- the hash of the immutable files is computed with `SHA256` of their binary content
- the message is computed with `SHA256` of the concatenation of the hashes of the immutable files
- the last immutable file, the ledger state and the volatile can not be signed as they are not deterministically computed by the Cardano node.

The message computation is the same on the signers and the aggregators.

[![Design of the certification of the Cardano node internal database](./images/cardano-node-database/message.jpg)](./images/cardano-node-database/message.jpg)
<small><center>Message creation on the signers and aggregators</center></small>

### Authenticity verification

The verification process operates on the full Cardano node internal database:

- the client downloads the a compressed artifact from an untrusted source (e.g. an aggregator or a cloud service)
- the client computes the message from the downloaded artifact and verifies that it is signed by a valid Mithril certificate.

[![Design of the certification of the Cardano node internal database](./images/cardano-node-database/message.jpg)](./images/cardano-node-database/message.jpg)
<small><center>Message creation on the clients (same as on signers and aggregators)</center></small>
