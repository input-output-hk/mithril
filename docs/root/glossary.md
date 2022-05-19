---
sidebar_position: 1
---

# Glossary

Here is a comprehensive list of terms'definition used in the project.

## Aggregator

An Aggregator is a software instance that computes, checks the [mithril signatures](#mithril-signature) and issues the [snapshots](#snapshot) along with their [digest](#snapshot-digest).

## Beacon

A Beacon is a software data structure that holds parameters tied to a given [epoch](#epoch).

## Certificate

A Certificate is a digital document that contains:

 * the Epoch when this information is relevant
 * the snapshot digest
 * the Mithril Signature of the digest
 * 

## Client

A client is a software ran by a Wallet owner or anyone who needs to compute the [ledger state](#ledger-state). Clients use Mithril to ensure the snapshot they download is certified by a randomly distributed population to be the official one.

## Digest

See [snapshot digest](#snapshot-digest).

## Epoch

Cardano uses Epochs to group blocks computed in a certain amount of time. Each epoch corresponds to certain security parameters. This makes harder to lead brut force attacks on the Blockchain.

## Immutable File

An Immutable File represents a fixed size chunk of the Blockchain state. It is immutable by essence since blocks computed in the past are immutables. Theses files are indexed by a sequential number in their filename.

## Ledger State

The Ledger State represents all the wallets'balances according to the transactions stored in the Blockchain. Ledger states are regulary computed at the end of each [epoch](#epoch).

## Mithril Signature

A Mithril Signature is a unique signature computed from a random distributed population of [signers](#signer)'s signatures depending on a common shared message.

## Party

A party is a person or something represented by a key pair on the Cardano network and who is involved in a [mithril signature](#mithril-signature).

## Signer

A Signer is a software that acts as a party in [mithril signatures](#mithril-signature). As such it uses the [party](#party)'s stake to participate to internal Mithril gambling process.

## Signer Verification Key

A Signer Verification Key is a key computed from a [Party](#party)'s private key to assess his identity in the [Mithril Signature](#mithril-signature).

## Snapshot

A Mithril Snapshot is a signed archive of the Blockchain that can be used by [Clients](#client) to restore the [Ledger State](#ledger-state) quicker than computing it from scratch.

## Snapshot Certificate

The snapshot certificate is a [mithril signature](#mithril-signature) of the [snapshot digest](#snapshot-digest) that ensures its authenticity to all [clients](#clients) who want to use it.

## Snapshot Digest

A snapshot digest (or simply, Digest) is a unique control sum (hash) associated to a [Snapshot](#snapshot). It can be used to assert a [Snapshot](#snapshot) is valid or can be signed to certify the according [Snapshot](#snapshot)'s provenance.

## Stake Distribution

The Stake Distribution is the list of [signers](#signer) of a Mithril signature with their stake.