---
sidebar_position: 1
---

# Glossary

Here is a comprehensive list of definitions for some common terms used in this guide.

## Beacon

A **Beacon** represents a point of the Blockchain for which a [**Mithril Certificate**](#certificate) is created. It embeds at least the version of the [**Cardano Network**](#cardano-network) that is targeted, and the associated [**epoch**](#epoch) and [**immutable file number**](#immutable-file-number).

## Cardano Network

The **Cardano Network** is a **Proof-of-Stake** Blockchain platform that supports the ADA cryptocurrency.

> More information is available [here](https://docs.cardano.org/introduction)

## Cardano Node

A **Cardano Node** is a node that runs in a [**Cardano Network**](#cardano-network). There are several types of nodes, among them are **Cardano Full Nodes** that hold a copy of the whole Blockchain. They can be used by **Wallets**, [**Stake Pool Operator**](#stake-pool-operator-spo), **Exchanges** or **Dapps**. One of the use cases of the [**Mithril Network**](#mithril-network) is to bootsrap rapidly a **Cardano Full Node**.

## Cardano Key Pair

A **Cardano Key Pair** is an asymmetric key pair used to identify a [**Stake Pool Operator**](#stake-pool-operator-spo) on the [**Cardano Network**](#cardano-network).

## Certificate

The **Mithril Aggregator** combines the produced [**multi signature**](#multi-signature) and some metadata into a [**Mithril Certificate**](#certificate) that will be later used by the [**Mithril Client**](#mithril-client) to verify the authenticity of a [**snapshot**](#snapshot). The certificates are chained so that the [**stake distribution**](#stake-distribution) used to create the signatures is verifiably genuine.

> More information is available [here](./mithril/mithril-protocol/certificates.md)

## Epoch

The [**Cardano Network**](#cardano-network) uses **Epochs** to group blocks computed in a certain amount of time (approximately 5 days). It is part of the design of its **Proof-of-Stake** consensus **Ouroboros**. At the end of each epoch, the [**stake distribution**](#stake-distribution) of the ending epoch is computed.

## Immutable File Number

Inside a the database of a [**Cardano Node**](#cardano-node), the Blockchain state is stored in **Immutable files** which never change once committed. These immutable files are designed so that they are deterministically produced and thus are the same on any **Cardano Node**. These files are created by following an incremental number, the **Immutable File Number** and there are three different immutable files for each number (i.e. _chunk_, _primary_ and _secondary_). Only the files up to the penultimate **Immutable File Number** are considered as committed and final, the last **Immutable File Number** files are constantly evolving. The [**Snapshots**](#snapshot) produced by the [**Mithril Network**](#mithril-network) rely on these **immutable files**.

## Individual Signature

For each [**Beacon**](#beacon), the [**Mithril Signers**](#mithril-signer) will compute on their end a message representing the Blockchain state, and sign it with their **Verification Keys** in order to create an [**Individual Signature**](#individual-signature). Upon winning one or multiple lotteries, the **Mithril Signer** will be able to use this **Individual Signature** to participate in the creation of a [**Multi Signature**](#multi-signature).

> More information is available [here](./mithril/mithril-protocol/protocol.md)

## Mithril Aggregator

The **Mithril Aggregator** is a trustless node of the [**Mithril Network**](#mithril-network) that orchestrates the work of the [**Mithril Signer**](#mithril-signer) nodes and that gathers their [**individual signatures**](#individual-signature) to produce [**Mithril multi signatures**](#multi-signature) and their associated [**certificates**](#certificate).

It is also in charge of creating and storing the [**snapshot**](#snapshot) archive.

> More information is available [here](./mithril/mithril-network/aggregator.md)

## Mithril Client

The **Mithril Client** node of the [**Mithril Network**](#mithril-network) is used to restore a [**Cardano full node**](#cardano-node) by retrieving, from a [**Mithril Aggregator**](#mithril-aggregator), a remote [**snapshot**](#snapshot), its [**certificate**](#certificate) chain and by verifying their validity thanks to the Mithril cryptographic primitives.

> More information is available [here](./mithril/mithril-network/client.md)

## Mithril Network

In its current version, the **Mithril Network** is a network of nodes responsible for creating [**Snapshots**](#snapshot) and [**Certificates**](#certificate) that enable fast bootstrap of a [**Cardano Node**](#cardano-node). It runs on top of the [**Cardano Network**](#cardano-network).

> More information is available [here](./mithril/mithril-network/architecture.md)

## Mithril Protocol

The **Mithril Protocol** allows **stakeholders** in a **Proof-of-Stake** Blockchain network to individually **sign messages** that are aggregated into a **multi signature** which guarantees that they represent a minimum share of the total stakes.

> More information is available [here](./mithril/mithril-protocol/protocol.md)

## Mithril Signer

The **Mithril Signer** is a node of the [**Mithril Network**](#mithril-network) that works transparently on top of the [**Stake Pool Operator**](#stake-pool-operator-spo) Cardano nodes and which individually signs the ledger state.

> More information is available [here](./mithril/mithril-network/signer.md)

## Multi Signature

The **Mithril Multi Signature** is an aggregate of [**Individual Signatures**](#individual-signature) which guarantees that a minimum share of the total stakes has participated in its creation.

> More information is available [here](./mithril/mithril-protocol/protocol.md)

## Snapshot

A Mithril Snapshot is a signed archive of the Blockchain state that can be used by [**Mithril Clients**](#mithril-client) to restore a [**Cardano Full Node**](#cardano-node). It is uniquely identified by its fingerprint or **Digest** which is part of the message signed by the [**Mithril Network**](#mithril-network).

## Stake Distribution

The **Cardano Stake Distribution** is the list of all the [**Stake Pool Operators**](#stake-pool-operator-spo) **Pool Id** addresses and their associated **Stakes Share** of the total **Stakes** of the [**Cardano Network**](#cardano-network).

The **Mithril Stake Distribution** is the list of all the [**Stake Pool Operators**](#stake-pool-operator-spo) (that are running a [**Mithril Signer**](#mithril-signer)) **Pool Id** addresses, their associated **Stakes Share** of the total **Stakes** of the [**Cardano Network**](#cardano-network), and their signing [**Verification Key**](#verification-key).

## Stake Pool Operator (SPO)

A **Stake Pool Operator**, also known as a **SPO**, represents a party that holds (via delegation) **Stakes** in the [**Cardano Network**](#cardano-network). The stakes entitle it to participate in the block production thanks to the Cardano consensus mechanism.

## Verification Key

In order to create [**Individual Signatures**](#individual-signature), the [**Mithril Signers**](#mithril-signer) must register their signing public key: the **Verification Keys**. To garantee their genuineness, they are signed by the associated [**Cardano Key Pair**](#cardano-key-pair). It is worth mentioning that a [**Mithril Signer**](#mithril-signer) must be aware of the **Verification Keys** of all the other **Mithril Signers** in order to produce valid **Individual Signatures**.

> More information is available [here](./mithril/mithril-protocol/protocol.md)
