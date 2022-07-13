---
sidebar_position: 4
sidebar_label: Mithril Client
---

# Mithril Client Node

Welcome to the Mithril Client Node guide!

## Introduction

:::info

The **Mithril Client** node is used to restore a **Cardano full node** by retrieving, from a **Mithril Aggregator**, a remote snapshot, its certificate chain and by verifying their validity thanks to the Mithril cryptographic primitives.

:::

:::tip

* For more information about the **Mithril Protocol**, please refer to the [Protocol in depth](../mithril-protocol/protocol.md) page.

* For more information about the **Mithril Client**, please refer to the [Developer Documentation](../../manual/developer-docs/nodes/mithril-client.md) page.

:::

## Usecases

At a first glance, a **Mithril Client** can be used by any user that needs to restore and bootstrap rapidly a **Cardano full node**:

* A full node wallet such as **Daedalus**
* A node operator (SPO, Exchange, Dapp)

On the long run, the **Mithril Client** is intended to be incorporated in **Light Clients** and **Wallets**.

## Snapshot Artifacts Retrieval

The first operation that a **Mithril Client** does is to retrieve snapshot artifacts from a remote source.

The **Mithril Aggregator** is used as a provider for the **Snapshots** (at least their locations).

This is done upon a manual action of a user that has previously selected the snapshot to use (if multiple are available).

These artifacs are stored locally on a temporary location, are uncompressed if necessary and used to compute the message that should have been computed and signed by the **Mithril Signers**.

## Snapshot Verification

Along with the snapshot artifacts, the **Mithril Client** will download the associated **Certificate Chain** and for each **Mithril Certificate** verify that (in the following order):

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

## Snapshot Restoration

Once the snapshot artifacts are verified, the **Mithril Client** node will move them from the temporary location to the final (user specified) location.

At this point, the **Cardano Node** will take over, start using the artifacts. and hopefully start addng new blocks to the ledger!
