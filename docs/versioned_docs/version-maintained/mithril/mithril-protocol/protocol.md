---
sidebar_position: 1
sidebar_label: Protocol in depth
---

# Mithril Protocol in depth

:::info

The research paper `Mithril: Stake-based Threshold Multisignatures` is downloadable [here](https://iohk.io/en/research/library/papers/mithril-stake-based-threshold-multisignatures/).

:::

:::info New

:new: Interact with the **Mithril Protocol** by experiencing with our [protocol simulation](./simulation.md). This will help you understand how the participants interact to create a multi signature and what's the impact of the protocol parameters.

:::

:::tip

For more information about the **Mithril Network**, please refer to the [Architecture](../mithril-network/architecture.md) page.

:::

## Introduction

The full details of the Mithril protocol are provided in the published paper. This section is high-level description of
the protocol in layman’s terms.

In a nutshell, Mithril is a signature scheme that generates a certificate that convinces
the verifiers that a portion of the stake of a system has signed a message.

However, rather than taking the whole set of
stake holders, Mithril
"randomly selects" a subset of them, and requires a portion of that subset to provide a signature.

An analogy would be
that Mithril signers participate in a lottery which defines if they are entitled to sign a particular message. In case
they win this lottery, then they can go ahead and sign the message. Whenever a sufficient number of parties have won the
lottery (and submitted their respective signatures), a certificate can be computed.

## The phases of the protocol

The protocol has 3 different phases:

1. **Protocol Establishment phase** which determines the parameters that will be used by Mithril participants. Also,
   during this phase, the set of "allowed signers" is defined.
2. **Initialization phase** during which Mithril nodes generate and exchange keys,
3. **Operations phase** during which nodes sign and aggregate signatures of messages to produce certificates.

:::note Note

Note that all 3 phases require the set of parties (`P` in the paper) to be fixed. During the protocol establishment phase
three important parameters are generated:

* `m`, defines the number of "lotteries" that a single user can participate in to sign a message.
* `k`, defines the required number of single signatures to produce a valid certificate.
* `phi_f` (as denoted in the library) which can be interpreted as the chance of a signer to win a lottery.

:::

Now, let's assume that we want to generate a valid signature for message `msg` using a mithril signature. Then we need `k` valid signatures from the subset of signers.

To this end, each player is allowed to participate in `m` "lotteries" to see if it "wins" the right to sign the message.

If none of this lotteries are won by this particular signer, then it will not be able to submit a signature.

If, contrarily, the signer wins one of the lotteries (or more than one), then it
can submit the corresponding signature (or all of the corresponding signatures respectively) to produce the valid certificate. The chance of winning the lottery is defined by a function over `phi_f`. The closer `phi_f` is to one, the higher the chances of winning a lottery are.

This means that there exists the possibility that a particular message cannot be certified using Mithril under a given set of parameters. There could be a scenario where not enough signers have won the allowed lotteries to produce a valid certificate. In which case the parameters can be adapted.

### 1. Protocol Establishment Phase

* Select/fix a prime order group to be used as basis for multi-signature scheme
* _When using bulletproof_: Generate a "random" string, for example by taking some unpredictable data from the net (hash
  of latest block, stockmarket quotes) and hash it
* When requested by a party, return the parameters and random string

This setup phase must be done only once and is valid for as long as the same proof system, e.g the parameters can very well be statically baked into the various systems producing and consuming Mithril proofs.

Similarly, during the protocol establishment phase, the parameters `k`, `m` and `phi_f` are defined, as well as the set of allowed signers.

### 2. Initialisation Phase

This phase includes both the actual initialisation phase of each party, and the registration of keys.

* Party retrieves the protocol parameters
* It uses the parameters to generate a new key pair of a verification key and a signing key, plus a Proof-of-possession
  `κ`. The verification key and PoP are broadcast to all other parties for registration, which is expected to last for
  some limited period of time. An important difference with the paper is that registration is not centralised in the "
  idealised" manner described in the paper. Instead, all signers will register against all other signers. Meaning that
  each signer will trace it's own registration procedure. Together with other participants' keys, the stake of each
  party is stored.
* Then an aggregate verification key (`AVK` in the paper) is created from the registration material, in the form of a
  Merkle-tree.

The key dissemination process can also happen on-chain, which makes sense as the parties need to have some guarantees about the validity of keys and stakes of each other party.

This phase happens once for each _instance_ of the protocol running.

### 3. Operations Phase

Operations run in cycles, where each cycle is triggered by a message (eg. a snapshot of the UTxO set) to sign for which
a quorum of `k` valid signatures must be submitted.

* Each party needs to check for eligibility to sign the message. It may be eligible to sign the same message more than once. This check is performed for `m` distinct indices. For every index, the signer evaluates a function (parameterized with `phi_f`) that takes its stake as input. If the result of the function is `SUCCESS`, then the signer is eligible to sign this message for the given index.

* For every valid signature, it creates a proof (`π`) containing a signature of the message, verification key, stake and paths of party in the Merkle-tree.

* Then, multiple signatures can be aggregated together to form a certificate (`τ`) by:
  * Verifying signatures from each party:
    * Checking the party is authorized to sign for the given index (using the same procedure as the signing)
    * Checking the proof is valid which means:
      * Check the evaluation threshold is correct for party’s stake given the message, index and signature
      * Check the provided path exists in the aggregate keys' Merkle-tree
      * Verify the signature of the message is valid w.r.t to verification key
  * Producing an aggregation key from all verification keys
  * Producing an aggregate signature from all signatures `μ`
  * Producing a proof using the aggregate keys, the message and the vector of individual proofs from each party:
    * In the concatenation proof system all these values are simply packed together to form the proof,
    * In the case of _bulletproof_ system, a more compact proof is generated.

* Each certificate `τ` can be verified to be valid for some message, using the known setup parameters to verify the certificate’s proof and then verifying the aggregate signatures and verification keys.

:::note Note

Note that if the individual signatures are broadcast to all parties, then each party can independently produce the certificates. In particular, the party that performs aggregation is not required to have any specific knowledge, nor it is assumed to be honest. This means that _any_ third party that has access to the individual signatures can perform the signature aggregation.

:::

:::tip

For more information about the **Mithril Network**, please refer to the [Architecture](../mithril-network/architecture.md) page.

:::
