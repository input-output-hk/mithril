---
sidebar_position: 2
sidebar_label: Certificate Chain in depth
---

# Mithril Certificate Chain in depth

## Introduction

The **Mithril Protocol** can be summarized as:

> A protocol that allows stakeholders in a Proof-of-Stake blockchain network to individually sign messages that are aggregated into a multi signature which guarantees that they represent a minimum share of the total stakes.

The **stake distribution** that is used to create the multi signature can't be trusted "as is" and must also be signed. Indeed, any one could relatively easily create a **Fake Stake Distribution** and use it to produce a valid multi signature that would be embedded in a perfectly **Valid but Non Genuine Certificate**. This certificate could be served by a dishonest **Mithril Aggregator** node and thus could lead to restoring a non genuine snapshot by a honest **Mithril Client**.

In order to avoid this type of **eclipse attack**, the **stake distribution** used to create a multi signature must be certified beforehand. This is where the **Certificate Chain** takes place.

## The Certificate Chain Design

:::danger

The **Stake Distribution** of an epoch is computed by the **Cardano Nodes** at the end of this epoch. It will be usable from the beginning of the following epoch.

:::

A way to certify the **stake distribution** used to create a multi signature is to verify that it has been previously signed in an earlier certificate. Then one can recursively verify that the earlier certificate is valid the same way. This process can be designed as a **Chain** of certificates: the **Mithril Certificate Chain**.

As multiple certificates may be created during the same epoch (i.e. with the same stake distribution), it is sufficient to link to only one certificate of the previous epoch: this will allow faster verification times and it will also avoid network congestion.

Also, the certificate at the beginning of the **Certificate Chain** has a special role. This is the **Genesis Certificate**. The only way to validate the stake distributon embedded in the **Genesis Certificate** is to sign it with a private key that is associated to a broadly available public key: the **Genesis Keys**. These are the keys that sign the hard forks used during the era transitions of the Cardano Blockchain.

Here is a diagram illustrating the **Certificate Chain** design:
[![Certificate Chain Design](images/certificate-chain.jpg)](images/certificate-chain.jpg)

Where the following notations have been used:

* `C(p,n)`: Certificate at trigger `p` and epoch `n`
* `FC(n)`: First Certificate of epoch `n`
* `GC`: Genesis Certificate
* `H()`: Hash
* `SD(n)`: Stake Distribution of epoch `n`
* `VK(n)`: Verification Key at epoch `n`
* `AVK(n)`: Aggregrate Verification Key at epoch `n` such as `AVK(n) = MKT_ROOT(SD(n) || VK(n))`
* `MKT_ROOT()`: Merkle-tree root
* `BEACON(p,n)`: Beacon at trigger `p` and epoch `n`
* `METADATA(p,n)`: Metadata of the certificate at trigger `p` and epoch `n`
* `MSG(p,n)`: Message of the certificate at trigger `p` and epoch `n`
* `MULTI_SIG(p,n)`: Multi signature created to the message `H(MSG(p,n) || AVK(n-1))`
* `GENESIS_SIG(MSG)`: Genesis Signature, i.e. signature of `MSG` with the Genesis Keys

The hash of a **Certificate** `H(C(p,n))` is computed as the concatenation (`||`) of all its fields. Therefore, if one field is modified, its hash is different.

The informations embedded in the `METADATA(p,n)` field are:

* The version of the Mithril Protocol
* The parameter of the Mithril Protocol (`k`, `m`, and `phi_f`)
* The date and time at which the multi signature creation was initiated
* The date and time at which the Certificate was sealed
* The list of the signers that actively contributed to the multi signature

The message `MSG(p,n)` is a map of multiple values associated with their respective keys and provides a way to add more informations in the certificates without breaking the chain itself. These can be any message that the signers are able to compute deterministically thanks to the Cardano consensus: an immutable files snapshot, the Utxo set, the stahe distribution, ...

:::note

The **trigger** represents the instant at which a certificate should be created. It is combined, with at least the associated **epoch** to create the [**Beacon**](../../glossary.md#beacon) of the certificate. In the current implementation, this trigger is a new [**Immutable File Number**](../../glossary.md#immutable-file-number). The epoch creation and the trigger creation processes may be uncorrelated.

:::

:::info

The `AVK` or **Aggregate Verification Key** is the root of the Merkle-tree where each leaf is filled with `H(STAKE(signer) || VK(signer))`. It represents the corresponding **Stake Distribution** in a condensed way.
:::

## The Verification Algorithm

The **Certificate Chain** verification can be stated as:

```
CHAIN_VERIFY[C(p,n(p))] = CERT_VERIFY[C(p,n(p)] ^ CERT_VERIFY[FC(n(p))] ^ CERT_VERIFY[FC(n(p)-1)] ^ ... ^ CERT_VERIFY[FC(1)] ^ CERT_VERIFY[GC]
```

Where the following notations have been used:

* The epoch `n(p)` depends on the trigger `p`
* `CHAIN_VERIFY[]`: Verify all the chain backward from a certificate
* `CERT_VERIFY[]`: Verify a specific certificate

A **Certificate Chain** is valid if from a **Certificate**, up to the **Genesis Certificate** of the chain there is at least one valid certificate per epoch.

A non **Genesis Certificate** is valid **if and only if** the `AVK` used to verify the multi signature is also part of the signed message used to create a valid multi signature in a previously sealed Certificate.

The **Genesis Certificate** is valid **if and only if** its **Genesis Signature** is verified with the advertised **Public Genesis Key**.

An implementation of the algorithm would work as follows for a certificate:

* **Step 1**: Use this certificate as the `current_certificate`
* **Step 2**: Verify (or fail) that the `current_hash` of the `current_certificate` is valid (by computing it and comparing with the `hash` field of the certificate)
* **Step 3**: Get the `previous_hash` of the `previous_certificate` by reading its value in the `current_certificate`
* **Step 4**: Verify (or fail) that the `multi_signature` of the `current_certificate` is valid
* **Step 5**: Retrieve the `previous_certificate` that has the hash `previous_hash`:
  * **Step 5.1**: If it is not a `genesis_certificate`:
    * **Step 5.1.1**: Verify (or fail) that the `previous_hash` of the `previous_certificate` is valid (by computing it and comparing with the `hash` field of the certificate)
    * **Step 5.1.2**: Verify (or fail) that the `current_avk` of the `current_certificate` is part of the message signed by the multi signature of the `previous_certificate`
    * **Step 5.1.3**: Verify (or fail) that the `multi_signature` of the `previous_certificate` is valid
    * **Step 5.1.4**: Use the `previous_certificate` as `current_certificate` and start again at **Step 2**
  * **Step 5.2**: If it is a `genesis_certificate`:
    * **Step 5.2.1**: Verify (or fail) that the `previous_hash` of the `previous_certificate` is valid (by computing it and comparing with the `hash` field of the certificate)
    * **Step 5.2.2**: Verify (or fail) that the `current_avk` of the `current_certificate` is part of the message signed by the genesis signature of the `previous_certificate`
    * **Step 5.2.3**: The certificate is valid (Success)

## The coexistence of multiple Certificate Chain

What would happen if some **Mithril Aggregator** claims that not enough signatures were received? This doesn’t really matter, as there will be a different **Mithril Aggregator** that would collect sufficient signatures and aggregate them into a valid Certificate.

Similarly, different **Mithril Aggregators** might have different views of the **individual signatures** submitted (one aggregator might receive 10 signatures, and a different one could receive 11), which would result in different **Certificates** signing the same message.

This would result in different **Certificate Chains** that would all link back to the **Genesis Certificate**. Indeed they would be represented by a tree of certificates where each traversal path from the root to a leave represent a valid **Certificate Chain**.

## The need of backward compatibility

The **Certificate Chain** is designed to last. At a certain point, we will need to handle the verification of the multi signature from legacy versions of the **Mithril** cryptographic library.

In order to achieve this backward compatibility, some options are available:

* Handle multi signature verification functions of legacy versions
* Recreate Genesis Certificates from time to time
* Create intermediate **Milestones Certificates** (with both a multi signature and a genesis signature)
* Design a format migration algorithm
