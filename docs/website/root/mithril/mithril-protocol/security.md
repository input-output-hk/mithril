---
sidebar_position: 3
sidebar_label: Security
---

# Mithril protocol security

:::info

Mithril is based on the research paper [Mithril: Stake-based Threshold Multisignatures](https://iohk.io/en/research/library/papers/mithril-stake-based-threshold-multisignatures/).

:::

Mithril is a stake-based threshold multisignature (STM) protocol designed to aggregate individual signatures into a compact certificate. This process is conditional upon the total stake supporting a given message surpassing a predefined threshold. The protocol achieves scalability in signing, communication, and verification by pseudorandomly selecting a subset of participants eligible to sign each message.

This document provides an in-depth security analysis of Mithril, focusing on potential threats and the protocol's defenses against various attack vectors. We begin with a concise overview of the STM protocol and the adversarial model, followed by a detailed discussion of its security mechanisms against common cryptographic attacks. Finally, we emphasize the importance of parameter selection, supported by examples illustrating the trade-offs between security and efficiency.

## Mithril protocol in a nutshell

The STM protocol allows multiple participants to collectively sign a message, where the total stake of the participants determines the validity of the signature.
It leverages threshold multisignatures, which enable the aggregation of multiple individual signatures into a single, compact signature.
This is particularly useful in PoS systems where the security of the blockchain depends on the distribution and control of stake among participants.

- _Threshold Multisignature_: A scheme where individual signatures from multiple participants can be aggregated into a single signature if the total stake of the participants exceeds a certain threshold.
- _Stake-based Eligibility_: The protocol ensures that only participants with a certain stake are eligible to sign messages, and this eligibility is determined pseudorandomly.
- _Aggregation and Verification_: Individual signatures are aggregated into a single multisignature, which can be verified efficiently.

### Protocol phases

- **Initialization Phase**

  - _Setup_: The protocol sets up necessary cryptographic parameters and prepares the system for operation.
    - _Key Generation_: Participants generate a public-private key pair $(sk_i, pk_i)$.
    - _Proof of Possession_: Each participant creates a proof $(\mathcal{PoP_i})$ that they possess the private key corresponding to their public key.
    - _Registration_: Participants register their public keys $(pk_i)$ and $(\mathcal{PoP_i})$, which are then stored in a Merkle tree structure for efficient verification.
    - _Aggregate Verification Key_: The root of the Merkle tree, which serves as the aggregate verification key $(\mathcal{AVK})$.

- **Operation Phase**
  - _Eligibility Determination_:
    - _Lottery Mechanism_: For each message, the protocol initiates a series of lotteries to determine eligible participants. Each participant's chance of winning is proportional to their stake.
    - _Security Parameter_ $(m)$: The number of parallel lotteries, ensuring enough participants are eligible.
    - _Quorum Parameter_ $(k)$: The minimum number of eligible signatures required to form a valid multisignature.
  - _Signing Process_:
    - _Individual Signature Generation_: Eligible participants generate individual signatures for the message.
    - _Aggregation_: These signatures are aggregated into a single multisignature. A minimum of $k$ signatures are aggregated into a single multisignature.
    - _Verification_: The multisignature, along with the Merkle proofs, is verified using the $\mathcal{AVK}$.

## Adversarial model

The adversarial model specifies the capabilities and goals of potential attackers.
For Mithril, we consider the following:

- **Security Assumptions**: The assumptions under which the security of Mithril is analyzed:
  - _Majority Honest Stake_: It is assumed that the majority of the stake is controlled by honest participants who follow the protocol.
  - _Unique Signature Scheme_: The signature scheme used is secure and ensures that only one valid signature can be produced per message for each verification key.
  - _Random Oracle Model_: The analysis assumes the availability of secure hash functions modeled as random oracles for certain security proofs.
- **Security Goals**:
  - _Integrity_: Ensure that only valid and legitimate participants can generate certificates, and that these certificates accurately reflect the consensus of the stakeholders.
  - _Resistance to attacks_: Ensure that the influence in the protocol is proportional to the stake held, making it difficult for an adversary to gain control through multiple fake identities.
  - _Forgery Resistance_: Prevent the adversary from creating valid forged signatures or concatenated proofs, particularly those that could lead to double-spending or chain reorganization.
  - _Long-Range attack Resistance_: Ensure that old stakes cannot be used to create an alternate certificate chain that could overwrite the current chain.
- **Adversary Capabilities**:
  - _Computational Power_: The adversary may have significant computational resources but is assumed to be polynomially bounded (standard assumption in cryptographic protocols).
  - _Stake Control_: The adversary may control a portion of the total stake in the system, potentially through collusion with other stakeholders or by compromising private keys.
  - _Identity Control_: The adversary may attempt to create multiple identities to gain influence in the protocol, though the protocol assumes this is economically challenging due to the stake-proportional selection mechanism.
  - _Forgery Attempts_: The adversary may try to forge concatenated proofs or signatures, manipulate eligibility checks, or create alternate certificate chains using old stakes.
- **Adversary Goals**:
  - _Forge a Certificate_: The adversary may attempt to generate a forged certificate by manipulating the signing process or controlling sufficient stake. Forged certificate would allow the adversary to falsely certifiy the information, compromising the protocol integrity.
  - _Prevent Certificate Production_: The adversary might attempt to prevent the legitimate production of a certificate by disrupting the participation of eligible signers or introducing conflicts that prevent successful aggregation. This could halt the certificate generation process.
  - _Manipulate Stake Distribution or Lottery Mechanism_: The adversary may try to exploit lottery mechanism by grinding attacks or manipulate the stake distribution to increase their chance of being selected. It could influence the certificate generation process.

## Signature and Proof Integrity attacks

In this section, we group _Concatenation Forgery_ and _Collusion_ attacks under signature and proof integrity attacks and discuss Mithril security regarding these attacks. Please see Appendix for further details.

Concatenation Forgery attacks involve an adversary attempting to forge a valid concatenated proof by manipulating or assembling parts of valid signatures from different contexts. Collusion attacks occur when multiple participants conspire to manipulate the system, potentially leading to consensus disruption or double-spending. Although these attacks differ in approach, they both target the protocol's signature and proof mechanisms.

### Mithril security mechanisms

1. **Contradiction soundness**:
   - Mithril employs a concatenation-based proof system where proofs are verified by checking the relation $R(x,w) = 1$.
   - The key feature, contradiction soundness, ensures that any attempt to manipulate concatenated proofs results in contradictions.
   - These contradictions are detectable by the verifier, thereby preventing the success of forgery attempts.
   - Colluding participants cannot generate contradictory proofs without being detected, as the contradiction soundness feature makes it impossible to combine valid signatures from different contexts without producing inconsistencies.
2. **Unique signature scheme**:
   - The unique signature scheme in Mithril guarantees that a verification key can produce only one valid signature $(\sigma)$ for any given message.
   - This prevents adversaries from assembling multiple valid signatures into a forged concatenated proof.
   - Colluders are also restricted by this unique signature property, as they cannot produce multiple valid signatures for the same message under the same key. This ensures that even coordinated attempts to manipulate the signing process are thwarted.
3. **Verification key and stake binding**:
   - Mithril binds each verification key to a specific stake.
   - Only signatures associated with valid and registered stakes are considered during verification.
   - This binding prevents adversaries from using unregistered or invalid keys to forge concatenated proofs.
   - The same binding mechanism hinders collusion by ensuring that any signatures produced by colluders must come from keys and stakes that are verifiably legitimate. This reduces the risk of colluders influencing the protocol with forged or manipulated signatures.
4. **Threshold mechanism**:
   - Mithril requires a threshold number of signatures to create a valid multisignature.
   - This threshold mechanism ensures that colluders must control a significant portion of the network's stake to generate a valid signature.
   - This makes large-scale collusion difficult and economically unfeasible.
   - The threshold mechanism also indirectly supports resistance to concatenation forgery by ensuring that only a valid, sufficiently large group of participants can produce the required signatures for a valid proof.
5. **Lottery mechanism and stake proportionality**:
   - The protocol uses a pseudorandom lottery mechanism to select signers based on their stake.
   - This selection process is unpredictable and stake-proportional, making it challenging for colluding participants to ensure they are selected together.
   - The proportionality of stake influence ensures fairness and security by requiring colluders to hold a large stake to affect outcomes significantly.
   - While primarily aimed at preventing collusion, the lottery mechanism also reinforces security against forgery by ensuring that only legitimately selected participants contribute to the proof process.
6. **Identity verification and protocol transparency**:
   - Mithril's identity verification process, using the $\mathcal{AVK}$ and $\mathcal{PoP}$, ensures that only legitimate participants with valid stakes and keys can participate.
   - The protocol's transparency allows for auditing and detection of irregularities, further deterring both forgery and collusion.
   - By verifying identities and maintaining transparency, the protocol minimizes the risk of both collusion and concatenation forgery.
   - Any suspicious activity or irregular combination of signatures can be identified and addressed promptly.

Mithril’s security mechanisms are designed to resist both Concatenation Forgery and Collusion attacks by leveraging contradiction soundness, a unique signature scheme, and strict identity verification processes. These defenses work together to ensure that any attempt to manipulate the protocol’s signatures or proofs, whether through forgery or collusion, will be detected and invalidated. By addressing these threats in a unified manner, Mithril maintains the integrity of its signature and proof processes, ensuring the overall security and robustness of the protocol.

## Identity and Selection Manipulation attacks

This section groups _Grinding_ and _Sybil_ attacks under identity and selection manipulation attacks. Mithril security mechanisms are discussed in terms of these kind of attacks. Please see Appendix for further details.

Grinding attacks involve an adversary repeatedly manipulating cryptographic processes, such as key generation or stake distribution, to increase their chances of being selected as a validator. Sybil attacks, on the other hand, involve creating multiple fake identities to gain disproportionate influence within the network. Both attacks target the protocol's fairness and integrity, either by gaming the selection process or overwhelming the network with fake participants.

### Mithril security mechanisms

1. **Robust randomness**:
   - Mithril utilizes cryptographic random number generators (RNGs) to ensure that the selection process for validators is both fair and unpredictable.
   - The pseudorandom selection of participants is based on their stake, but the outcome is independent of the specifics of stake distribution.
   - This randomness prevents an adversary from manipulating the process through key or stake grinding, as the probability of selection remains resistant to such exploitation.
   - While primarily aimed at grinding, robust randomness also helps mitigate Sybil attacks by ensuring that even if multiple identities are created, the selection process remains fair and cannot be easily manipulated by simply increasing the number of identities.
2. **Identity verification**:
   - During each epoch's registration process, participants must provide $\mathcal{PoP}$ to demonstrate control over the private key corresponding to their verification key.
   - This requirement ensures that each identity in the protocol is backed by a legitimate cryptographic key, making it difficult for an adversary to create numerous fake identities without the corresponding cryptographic proof.
   - $\mathcal{PoP}$ also plays a role in preventing grinding attacks, as it ties each registered identity to a verifiable cryptographic proof, preventing the adversary from easily generating or manipulating keys to increase their chances of selection.
3. **Stake-proportional eligibility**:
   - In Mithril, the probability of being selected to sign is proportional to the participant's stake.
   - This discourages the creation of multiple identities with small stakes, as their collective influence remains limited.
   - The protocol favors concentrated stakes, making it economically impractical to launch a Sybil attack by distributing stake across many identities.
   - The stake-proportional eligibility mechanism also counters grinding attacks, as generating multiple low-stake keys does not significantly increase the adversary's selection chances, ensuring that the selection process remains fair and stake-dependent.
4. **Merkle tree and aggregate verification key**:
   - All registered public keys and their associated $\mathcal{PoP}$s are organized into a Merkle tree, with the root known as the $\mathcal{AVK}$.
   - This structure ensures efficient and secure verification of each participant's identity.
   - By verifying each identity against the $\mathcal{AVK}$, the protocol prevents the inclusion of fake or manipulated identities in the signing process.
   - The Merkle tree structure ties participants' identities to their stakes, ensuring that the eligibility determination is based on verified and legitimate identities.
   - This mitigates both grinding and Sybil attacks by securing the verification process against manipulation.
5. **Registration and epoch-based validation**:
   - Participants must register their stakes and keys at the beginning of each epoch.
   - This registration process, which includes verifying the participant's stake and associated $\mathcal{PoP}$, ensures that only active and valid participants can take part in the protocol.
   - This mechanism prevents stale or fake identities from persisting across epochs without revalidation.

Mithril's security mechanisms are carefully designed to resist both Grinding and Sybil attacks by employing robust randomness, thorough identity verification, and stake-proportional selection processes. These defenses work in tandem to ensure that the protocol remains secure against attempts to manipulate identity and selection mechanisms, whether through generating multiple keys or creating fake identities. By addressing these threats together, Mithril maintains the integrity and fairness of its consensus process, ensuring that only legitimate participants have influence over the network.

## Long Range attacks

This section covers the security mechanisms of Mithril against long range attacks. Please see Appendix for further information.

Mithril is designed to safeguard the network against _Long Range attacks_, where an adversary attempts to exploit old, valid keys or stakes to create an alternate certificate chain from a distant point in the past. These attacks can compromise the integrity of the system by producing outdated or invalid certificates that undermine the current state. Mithril employs several mechanisms to counter this threat, ensuring the security and stability of the protocol.

#### Mithril security mechanisms

1. **Epoch-based checkpoints**:
   - Mithril divides the certificate chain into epochs, each serving as a natural checkpoint.
   - At the end of every epoch, participants generate signatures that are aggregated into a certificate, which acts as a finalized record of the epoch's events.
   - These epoch-based certificates ensure that the certificate chain's history is fixed and verifiable at regular intervals.
   - This checkpoint system makes it highly difficult for an adversary to create a valid alternate certificate chain that diverges from the legitimate chain before the last checkpoint.
   - Any attempt to do so would require reconstructing an entire epoch's worth of valid signatures, which is practically infeasible.
2. **Finality of certificates**:
   - Once a certificate is generated and accepted for an epoch, it represents a finalized and irreversible state of the protocol.
   - This strong notion of finality means that after a certificate is in place, it cannot be replaced or reorganized.
   - This finality mechanism ensures that even if an adversary attempts to create an alternate chain, it will be rejected if it conflicts with finalized certificates.
   - The protocol's design guarantees that once a state is finalized, it becomes a permanent part of the chain, protecting against reorganization attacks.
3. **Stale stake invalidation**:
   - Mithril requires participants to register their stakes and associated verification keys at the beginning of each epoch by providing $\mathcal{PoP}$.
   - This process ensures that only current, active participants are included in the epoch.
   - Stale stakes (old, inactive keys) are effectively invalidated because they are not re-registered.
   - If a participant fails to register for a new epoch, their previous $vk$ and stake are excluded from the new $\mathcal{AVK}$.
   - This prevents the use of old keys in generating certificates for the current epoch, ensuring that only legitimate and active stakes contribute to the protocol's security.
4. **Robust randomness in eligibility**:
   - The selection of participants for signing certificates is determined by a pseudorandom process influenced by current stakes.
   - This randomness ensures that the selection process is fair, secure, and unpredictable.
   - Even if an adversary possesses old keys, they cannot predict or influence the selection process for the current epoch.
   - This randomness makes it extremely difficult to manipulate the certificate creation process using outdated or compromised keys, further protecting the protocol against long range attacks.

Mithril employs a comprehensive set of mechanisms to protect against long range attacks, ensuring that the protocol remains secure even when faced with adversaries attempting to exploit old keys or stakes. By integrating epoch-based checkpoints, certificate finality, stale stake invalidation, and robust randomness, Mithril ensures that the certificate chain remains intact and resistant to reorganization or manipulation, thereby maintaining the integrity and stability of the network.

## Appendices

### Concatenation Forgery attacks

Concatenation Forgery attack refers to a type of cryptographic attack where an adversary attempts to forge a valid combined (or concatenated) proof or signature by manipulating or assembling parts of valid signatures or proofs from different sources. This type of attack is particularly relevant in protocols that involve aggregating or concatenating multiple cryptographic components to form a single, composite proof or signature.
Consider a protocol where multiple participants sign different parts of a message, and these signatures are concatenated to form a final, unified signature. In a concatenation forgery attack, an adversary might try to:

- Collect valid signatures from different messages or contexts.
- Reorder or selectively combine these signatures.
- Present the manipulated result as a valid signature for a different message.

### Collusion attacks

Collusion attacks occur when multiple participants in a network conspire to manipulate the system for their collective benefit, undermining the consensus mechanism and potentially leading to issues like double-spending or undue network control.

Collusion involves two or more parties working together to achieve a shared malicious goal. Manipulating the protocol's outcomes, such as block production or transaction validation. The attack aims to compromise the fairness and security of the network's consensus mechanism. Successful collusion can result in economic losses and weaken the network's security.

- _Double-Spending_: Colluders create a blockchain fork to spend the same assets multiple times.
- _Validator Collusion_: Validators work together to control block validation, influencing consensus decisions.
- _Majority Stake Collusion_: Controlling a majority of the network’s stake to perform a "$51\%$ attack."

### Grinding attacks

Grinding attacks in cryptography, particularly in PoS blockchains, involve an adversary repeatedly attempting to manipulate the cryptographic process (such as key generation or stake distribution) to increase their chances of being selected as a validator. By generating multiple keys or adjusting their stake, the attacker tries to gain an unfair advantage in the consensus mechanism.

- _Key Grinding_: The adversary generates numerous cryptographic keys, selecting those that are most favorable in the context of eligibility or winning a lottery-based selection process .
- _Stake Grinding_: The adversary repeatedly modifies their stake (e.g., by splitting it among multiple identities) to increase the probability of being chosen as a validator.

### Sybil attacks

A Sybil attack occurs when an attacker creates multiple fake identities within a decentralized network to gain undue influence or control. This undermines the integrity and fairness of the network by centralizing power in the hands of the attacker.

- The attacker generates many identities to appear as multiple independent participants.
- These identities allow the attacker to disproportionately affect decisions, such as in blockchain consensus or online voting.
- The attack weakens the decentralized nature of the network.
- Ensuring strong, verifiable identities reduces the risk of fake accounts.

### Long Range attacks

This section covers the security mechanisms of Mithril against long range attacks. Please see Appendix for further information.

A long range attack in Proof-of-Stake (PoS) blockchains involves an adversary using old, valid keys or stakes to create an alternate blockchain starting from a distant point in the past. The attacker builds this alternative chain until it surpasses the length of the legitimate chain, potentially leading to a chain fork or takeover.

- Old Keys/Stakes exploit previously valid, but now inactive, keys or stakes.
- The attacker creates a competing chain that eventually overtakes the current chain.
- Can result in double-spending or chain reorganization, undermining the network's integrity.

Long range attacks exploit the time-insensitivity of PoS systems, but these mitigation strategies help protect the network's security and stability.
