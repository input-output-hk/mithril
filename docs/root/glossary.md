---
sidebar_position: 1
---

# Glossary

Here is a comprehensive list of terms' definition used in the project.

## Aggregator

An Aggregator is a software instance that computes, checks the [mithril multisignatures](#mithril-multisignature) and issues the [snapshots](#snapshot) along with their [digest](#snapshot-digest).

## Beacon

A beacon is a group of parameters tied by a temporal period. Each time a beacon ends, a new Mithril Certificate is issued.

## Cardano Key Pair

A Cardano key pair is a asymmetric key pair used to identify a stake holder on the Cardano Network.

## Certificate

A Certificate is a digital document that contains:

 * the [Beacon](#beacon) when this information is relevant
 * the snapshot digest
 * the [Mithril multisignature](#mithril-multisignature) of the digest
 * a list of mirrors where the snapshot can be downloaded

## Client

A client is a software ran by a Wallet owner or anyone who needs to compute the [ledger state](#ledger-state). Clients use Mithril to ensure the snapshot they download is certified by a randomly distributed population to be the official one.

## Digest

See [snapshot digest](#snapshot-digest).

## Epoch

Cardano uses Epochs to group blocks computed in a certain amount of time. Each epoch corresponds to certain security parameters. This makes harder to lead brut force attacks on the Blockchain. At the end of each Cardano's epoch, the stake distribution is computed.

## Immutable File

An Immutable File represents a fixed size chunk of the Blockchain state. It is immutable by essence since blocks computed in the past are immutables. Theses files are indexed by a sequential number in their filename.

## Ledger State

The Ledger State represents all the wallets' balances according to the transactions stored in the Blockchain. Ledger states are regulary computed every time an [immutable file](#immutable-file) is generated.

## Mithril Multisignature

A mithril multisignature is a unique signature computed by a random distributed population of [signers](#signer)'s signatures represening a minimal stake share and  based on a common message.

## Mithril Single Signature

A Mithril single signature is a signatures derived from the Signer's verification key based on Mithril parameters and can be used in the [Mithril eultisignature](#mithril-multisignature) if this party wins one or more lotteries. 

## Party

A party is an entity that owns stake and is identified by a key pair (WHAT NAME?). A party can be involved in a [mithril multisignature](#mithril-multisignature). The public key is used as party identifier in the Mithril signature process.

## Signer

A Signer is a software that acts as a party in [mithril multisignatures](#mithril-multisignature). It represents a single contributor in the Mithril internal lottery mechanism hence he is owner of a [signer key pair](#signer-key-pair). 

## Signer Key Pair

When interacting with the Mithril protocol, each party must generate a asymmetrical key pair: signing (or private) key and verification (or public) key based on his own [Cardano key pair](#cardano-key-pair) added with stake information. 

## Snapshot

A Mithril Snapshot is a signed archive of the Blockchain that can be used by [Clients](#client) to restore the [Ledger State](#ledger-state) quicker than computing it from scratch.

## Snapshot Certificate

The snapshot certificate is a [mithril multisignature](#mithril-multisignature) of the [snapshot digest](#snapshot-digest) that ensures its authenticity to all [clients](#clients) who want to use it.

## Snapshot Digest

A snapshot digest (or simply, Digest) is a unique control sum (hash) associated to a [Snapshot](#snapshot). It can be used to assert a [Snapshot](#snapshot) is valid or can be signed to certify the according [Snapshot](#snapshot)'s provenance.

## Stake Distribution

The Stake Distribution is the list of [signers](#signer) of a [mithril multisignature](#mithril-multisignature) with their stake.