---
sidebar_position: 3
sidebar_label: Mithril Certificates Chain
---

# Mithril Certificates Chain in depth

This document covers an open discussion on some important details required to
build the certificates in general, and we give particular importance into how
to create the certificate chain. More specifically, this document presents some
ideas on the following topic:

* Role of the aggregator,
* Certificate granularity,
* Not collect enough signatures, and
* How messages to be signed are obtained,

Open questions:

* [ ] Define a rule to handle the case where we receive valid certificates for
two different messages in a single "mithril epoch".
* [ ] Define a rule to handle mithril skipping a full Cardano epoch. This is
unlikelly, but we should define what to do in that case.
* [ ] Is it an undesirable burden for users to produce themselves the message
to be signed?

## Role of the aggregator

What would happen if some aggregator claims not enough signatures were
received? This doesn’t really matter, as there will be a different
aggregator that will collect sufficient signatures and aggregate them
into a valid certificate. Similarly, different aggregators might have
different views of the signatures submitted (one aggregator might receive
10 signatures, and a different one could receive 11), which would result
in different certificates signing the same message. Again, this is not a
problem, as long as they certify the same message (we should specify
what to do if we receive more than one message signed in the same
"mithril epoch").

As a result, aggregator should not be responsible for distributing the
snapshot. There should be a separate entity that receives aggregated
certificates, and then distributes the snapshot.

## Certificate granularity

There is no requirements on the certificate granularity. However, it is
desirable that there always is _at least_ one certificate per new stake
distribution. Given that in Cardano the stake distribution is updated at
each epoch, we should aim towards guaranteeing one valid certificate per
epoch. However, given that certificate generation is pseudorandom, there
exists the possibility of skipping some certificates (we present below
the probability of this happening). Therefore, having more than one
certificate per epoch seems a good option, specially because when bootstrapping
a new node, one only needs to verify one certificate per epoch.

Recall that when you are bootstrapping you only really care about the
signature of the next stake distribution (and not about the state of the
node). Therefore, if we have more than one certificate per epoch, when we
are bootstrapping we don’t need to verify all these certificates. We only
need to verify the certificates that certify the next stake distribution,
and not those that maintain the distribution of the previous certificate.
A node that is bootstrapping only really needs to verify the first
certificate (or the last, really depends how we organise the message signed),
of each different stake distribution, hence of each epoch. The other
certificates are not required, as what a node is doing while bootstrapping
is verifying the stake changes to be able to validate the next certificate.
Therefore, even if we produce 10 certificates per epoch, when a node is
bootstrapping it can verify one certificate per epoch, until the last one,
where it verifies all existing certificates.

## Not enough signatures

As mentioned above, the creation of certificates is pseudorandom, so we
might find ourselves in a situation where the submitted signatures are not
enough. As presented in the paper, the goal is to make the probability of this
happening as small as possible, while guaranteeing that an adversary will not
be able to generate invalid certificates. Table 2 of the paper is interesting
to discuss this (it shows required values of k, n so that an adversarial
quorum is formed with P ≤ 2 ^{−128}). The table shows different values, but if
we take for instance φ(.55), and assume adversarial stake of 40%, the
probability of reaching a quorum is 99.999% (assuming adversarial stake does not
participate). If we consider an adversarial stake of 33%, then with φ(.60) we
would reach a quorum with probability of 99.667%. We can also think of adversarial
stake as nodes which are not available.
Nonetheless, what to do when these events happen, and we don’t have a certificate.
The paper mentions the possibility of using a counter:
"_if the probability of an honest quorum remains significant it can be boosted by
allowing retries (e.g by attaching a short counter to the message)_".  
However, there would also be the possibility of skipping a certificate, and
signing the next one. The probabilities of not reaching a quorum are so small
that it is unlikely that we will skip several blocks in a row. Skipping one (or
even a few) block is not problematic (as long as we have one per epoch, in the
opposite case we should reconsider what to do, as this becomes a bit uglier wrt
long range attacks).

The other mitigation approach is to have a sort of hybrid parametrization, where
we would run a big (k,m) pair and a small one at the same time.
If aggregators can find enough signatures in the small param set, they prefer
that, if not they use the larger params. Might be clunkier for rewards though.

### Signers know if not enough signatures were reached

If we allow signers to also be aggregators, and therefore listen and accumulate
signatures, then they will know how many are being received and whether the
quorum is being reached or not. If we use the index solution proposed above,
a signer could get a grasp of whether the quorum is going to be reached or not, and start
trying with the next index. If we don’t follow the index solution, then signers don’t really
care whether the quorum was reached or not, they simply proceed with the next signature.

## How messages are obtained

The message to be signed is chosen by the signer itself. It is not received
externally. If I’m a signer, I should have sufficient information to select
what message I need to sign--this is, the stake distribution and aggregate key,
and the state of the node at the block number for which we are producing a
signature. Given that choosing the message is a deterministic procedure,
all honest nodes will generate a signature for the same message. Following
this, there is no room for adversaries to request signatures for adversarial
messages, nor the need for signers to validate the message to be signed.

To guarantee that users have the same view of what is being signed, the
snapshots are created past _k_ blocks (security parameter), which already
gives high guarantees we have reached consensus.
