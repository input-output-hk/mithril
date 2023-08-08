---
sidebar_position: 4
sidebar_label: Mithril Client
---

# Mithril Client Node

Welcome to the Mithril Client Node guide!

## Introduction

:::info

The **Mithril Client** node is used to list, show or verify artifacts certified by **Mithril certificates**:

 1. Cardano full node snapshots
 1. Stake Distribution involved in Mithril signatures.

:::

:::tip

* For more information about the **Mithril Protocol**, please refer to the [Protocol in depth](../mithril-protocol/protocol.md) page.

* For more information about the **Mithril Client**, please refer to the [Developer Documentation](../../manual/developer-docs/nodes/mithril-client.md) page.

:::

## Wallet restoration

At a first glance, a **Mithril Client** can be used by any user that needs to restore and bootstrap rapidly a **Cardano full node**:

* A full node wallet such as **Daedalus**
* A node operator (SPO, Exchange, Dapp)

On the long run, the **Mithril Client** is intended to be incorporated in **Light Clients** and **Wallets**.

### Certificate chain Verification

The first thing the **Mithril Client** does is to download the associated **Certificate Chain** and for each **Mithril Certificate** verify that (in the following order):

1. The certificate is not tampered (by computing its hash and verifying that is is the same as the one used for downloading it).
2. The locally computed **message** is the same as in the certificate.
3. The **multi signature** of the certificate is valid and computed with the certificate **message**.
4. The stake distribution used to compute the multi signature is signed:
    * Into a multi signature of a previous certificate of the chain (if there is one available).
    * Or by a valid **Genesis Certificate** (in case this is the first certificate of the chain).

The **Mithril Aggregator** is used as a provider for the **Certificate Chain**.

:::tip

For more information about the **Mithril Certificate Chain**, please refer to the [Certificate Chain](../mithril-protocol/certificates.md) page.

:::

### Snapshot Artifacts Retrieval

Once the certificate chain is verified, the **Mithril Client** will try to download a **Full Cardano Node** snapshot. The **Mithril Aggregator** is used as a provider for the **Snapshots** locations. The snapshots might be stored at several locations, the client will try the given locations until it founds one that responds OK. 

These artifacts are downloaded locally on a temporary directory and then uncompressed in the location given on the command line. The uncompressed files are used to compute the message that is then compared with the one that is signed by the **Mithril Signers**. If the verification fails, the uncompressed files are removed from the disk.

### Snapshot Restoration

If the verification succeeds, the user can use these files to start a Cardano full node. At this point, the **Cardano Node** will take over and hopefully start adding new blocks to the ledger!

## Mithril Stake Distribution

The client can be also used to verify and download the **Stake Distribution** used for signatures at the upcoming Cardano's Epoch. As of Snapshots, the certificate chain is validated, then the signers **Verification Keys** are checked. If valid, a fingerprint of the stake distribution is computed and tested against the one used by the certificate. If it is different, the verification stops, otherwise, the JSON representation of the stake distribution is saved on the disk for further use.