---
sidebar_position: 4
sidebar_label: Mithril client
---

# Mithril client node

:::info

The Mithril client node is used to list, show or verify artifacts certified by Mithril certificates:

 1. Cardano full node snapshots
 1. Stake distribution involved in Mithril signatures.

:::

:::tip

* For more information about the Mithril protocol, see the [protocol in depth](../mithril-protocol/protocol.md) overview.

* For more information about the Mithril client, see the [developer manual](../../manual/developer-docs/nodes/mithril-client.md).

:::

## Wallet restoration

A Mithril client can be used by anyone who needs to rapidly restore and bootstrap a Cardano full node:

* A full-node wallet such as Daedalus
* A node operator (SPO, exchange, DApp).

In the long run, the Mithril client will be incorporated in light clients and wallets.

### Certificate chain verification

The initial action of the Mithril client involves downloading the corresponding certificate chain. For each Mithril certificate, the client proceeds to verify the following in the given order:

1. The certificate has not been tampered with (by computing its hash and verifying that it is the same as the one used for downloading it)
2. The locally computed message is the same as in the certificate
3. The multi-signature of the certificate is valid and computed with the certificate message
4. The stake distribution used to compute the multi-signature is signed:
    * By a multi-signature of a previous certificate of the chain (if there is one available)
    * Or by a valid genesis certificate (in case this is the first certificate of the chain).

The Mithril aggregator is used as a provider for the certificate chain.

:::tip

For more information about the Mithril certificate chain, see the [certificate chain](../mithril-protocol/certificates.md) overview.

:::

### Snapshot artifacts retrieval

Once the certificate chain is verified, the Mithril client will try to download a full Cardano node snapshot. The Mithril aggregator is used as a provider for the snapshot locations. The snapshots might be stored at several locations, the client will try the given locations until it finds one that responds well. 

These artifacts are downloaded locally in a temporary directory and then uncompressed in the location given on the command line. The uncompressed files are used to compute the message that is then compared with the one that is signed by the Mithril signers. If the verification fails, the uncompressed files are removed from the disk.

### Snapshot restoration

If the verification succeeds, the user can use these files to start a Cardano full node. At this point, the Cardano node will take over and start adding new blocks to the ledger.

## Mithril stake distribution

The client can be also used to verify and download the stake distribution used for signatures at the upcoming Cardano's epoch. As with snapshots, the certificate chain is validated, then the signers' verification keys are checked. If valid, a fingerprint of the stake distribution is computed and tested against the one used by the certificate. If it is different, the verification stops. Otherwise, the JSON representation of the stake distribution is saved on disk for further use.
