# Mithril Protocol

The full details of the Mithril protocols are provided in the published paper. This section is high-level description of the protocol in layman’s terms.

The protocol has 3 different phases:

1. Protocol Establishment phase (called Reference String functionality in the paper) which generates some parameters to be used by all Mithril network parties,
2. Initialisation phase during which Mithril nodes generate and exchange keys,
3. Operations phase during which nodes sign and aggregate signatures of messages to produce certificates.

Note that all 3 phases require the set of parties (P in the paper) to be fixed. Also, there are 2 other important parameters m and k which represent respectively a security parameter and a quorum.

> Question: How is this set defined? How are parties identified by all other parties? Should be some known set of parties identified by their keys?

## Protocol Establishment Phase

* Select/fix a prime order group to be used as basis for multi-signature scheme
* _When using bulletproof_: Generate a "random" string, for example by taking some unpredictable data from the net (hash of latest block, stockmarket quotes) and hash it
* When requested by a party, return the parameters and random string

This setup phase must be done only once and is valid for as long as the same proof system, e.g the parameters can very well be statically baked into the various systems producing and consuming Mithril proofs.

## Initialisation Phase

This phase includes both the actual initialisation phase of each party, and the registration of keys.

* Party retrieves the protocol parameters
* It uses the parameters to generate a new key pair of a verification key and a signing key, plus a Proof-of-possession κ. The verification key and PoP are broadcast to all other parties for registration, which is expected to last for some limited period of time
* When the registration period ends, all parties get sent the complete set of keys and store them along with the corresponding stake for each party. Then an aggregate verification key (AVK in the paper) is created from the registration material, in the form of a Merkle-tree

The paper hints at the fact the key dissemination process can also happen on-chain, which makes sense as the parties need to have some guarantees about the validity of keys and stakes of each other party.

This phase happens once for each _instance_ of the protocol running.

## Operations Phase

Operations run in cycles, where each cycle is triggered by a message (eg. a snapshot of the UTxO set) to sign for which a quorum of `k` out of `m` stake must contribute a valid signature.

* Each party needs to check for eligibility to sign the message, a check which is a function of its stake (ϕ in the paper), `k` and `m` resp. the expected stake threshold and the total stake. concretely, each party tries various values for an `index` varying between 1 and `m` until it finds _at least once_ which is valid
* It creates a proof (π) containing a signature of the message, verification key, stake and paths of party in the merkle-tree
* Then multiple signatures can be aggregated together to form a certificate (τ) by:
  * Verifying signatures from each party:
    * Checking the party is authorised to sign for the given index (using the same procedure as the signing)
     * Checking the proof is valid which means:
       * Check the evaluation threshold is correct for party’s stake given the message, index and signature
       * Check the provided path exists in the aggregate keys' Merkle-tree
       * Verify the signature of the message is valid w.r.t to verification key
  * Producing an aggregation key from all verification keys
  * Producing an aggregate signature from all signatures μ
  * Producing a proof using the aggregate keys, the message and the vector of individual proofs from each party:
    * In the concatenation proof system all these values are simply packed together to form the proof,
    * In the case of _bulletproof_ system, the random string from the setup phase is also used to produce a more compact proof
* Each certificate τ can be verified to be valid for some message, using the known setup parameters to verify the certificate’s proof and then verifying the aggregate signatures and verification keys.

Note that if the individual signatures are broadcast to all parties, then each party can independently produce the certificates.
