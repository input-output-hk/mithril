//! Ad-Hoc Threshold Multisignatures
//!
//! Proof-of-Stake Sidechains
//! <https://eprint.iacr.org/2018/1239.pdf>
//!
//! The implementation in this module is parameterized by the underlying
//! signature scheme, which will define its own type of key and signature. All
//! we need here is to be able to verify signatures against keys, and verify that
//! keys have a valid PoP.

use crate::{merkle_tree::*, stm::Stake};
use ark_ff::ToBytes;
use digest::Digest;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::io::Write;
use std::iter::Sum;
use std::ops::Sub;

/// The values that are represented in the Merkle Tree.
#[derive(Debug, Clone, Copy)]
pub struct MTValue<PK>(pub PK, pub Stake);

impl<PK: ToBytes> ToBytes for MTValue<PK> {
    fn write<W: Write>(&self, mut writer: W) -> std::result::Result<(), std::io::Error> {
        self.0.write(&mut writer)?;
        self.1.write(&mut writer)
    }
}

/// The implementation relies on an underlying public signature with a proof
/// of possession. We need the ability to verify the signature. Moreover, it is common that
/// multi signatures schemes require the verification of correctness of the public key, and
/// therefore we require also associate a `check_key` function.
pub trait Atms: Sized + Debug {
    /// Public key before verification of correctness.
    type PreCheckedPK: Copy;
    /// Public key after verification of correctness.
    type CheckedPK;
    /// Associated type that defines the Signature.
    type SIG;
    /// Verify the correctness of the public key. If it is valid, return the public key (without
    /// the proof of correctness).
    fn check_key(proof: &Self::PreCheckedPK) -> Option<Self::CheckedPK>;
    /// Verify that the signature is valid for a message `msg` and key `key`
    fn verify(msg: &[u8], pk: &Self::CheckedPK, sig: &Self::SIG) -> bool;
}

/// An aggregated key, that contains the aggregation of a set of keys, the merkle commitment of
/// this set of keys, a map mapping the position of a key within the merkle tree and the total
/// stake of the system.
#[derive(Debug)]
pub struct Avk<A, H>
where
    A: Atms,
    H: MTHashLeaf<MTValue<A::CheckedPK>> + Digest,
{
    /// The product of the aggregated keys
    aggregate_key: A::CheckedPK,
    /// The Merkle commitment to the set of keys
    tree: MerkleTree<MTValue<A::CheckedPK>, H>,
    /// Mapping to identify position of key within merkle tree
    leaf_map: HashMap<A::CheckedPK, (Stake, usize)>,
    /// Total stake
    total_stake: Stake,
    /// Threshold of Stake required to validate a signature
    threshold: Stake,
}

/// An Aggregated Signature
#[derive(Debug)]
pub struct Asig<A, F>
where
    A: Atms,
{
    /// The product of the aggregated signatures
    aggregate: A::SIG,
    /// Proofs of membership of non-signing keys in the original
    /// set of keys
    keys_proofs: Vec<(A::CheckedPK, Path<F>)>,
}

#[derive(Debug)]
/// Errors associated with Atms
pub enum AtmsError<A, F, H>
where
    A: Atms,
    H: MTHashLeaf<MTValue<A::CheckedPK>> + Digest,
{
    /// An unknown key (we don't know its index in the merkle tree)
    UnknownKey(A::CheckedPK),
    /// The proof that `PK` is at a given idx is false
    InvalidMerkleProof(usize, A::CheckedPK, Path<F>),
    /// The proof that the secret key with respect to `PK` is known, fails
    InvalidProofOfPossession(A::PreCheckedPK),
    /// Duplicate non-signers in signature
    FoundDuplicates(A::CheckedPK),
    /// Non-signers sum to the given stake, which is more than half of total
    TooMuchOutstandingStake(Stake),
    /// Underlying signature scheme failed to verify
    InvalidSignature(A::SIG),
    /// The given public key does not correspond to the aggregation of the given
    /// set of keys
    InvalidAggregation(Avk<A, H>),
}

impl<A, H> Avk<A, H>
where
    A: Atms,
    A::CheckedPK: Eq + Ord + Clone + Hash + for<'a> Sum<&'a A::CheckedPK> + ToBytes,
    H: MTHashLeaf<MTValue<A::CheckedPK>> + Digest,
{
    /// Aggregate a set of keys, and commit to them in a canonical order.
    /// Called `AKey` in the paper.
    pub fn new<F>(
        keys_pop: &[(A::PreCheckedPK, Stake)],
        threshold: Stake,
    ) -> Result<Self, AtmsError<A, F, H>> {
        let mut keys: Vec<(A::CheckedPK, Stake)> = Vec::with_capacity(keys_pop.len());
        let mut total_stake: Stake = 0;
        for (key, stake) in keys_pop {
            let pk = match A::check_key(key) {
                Some(p) => p,
                _ => return Err(AtmsError::InvalidProofOfPossession(*key)),
            };
            total_stake += stake;
            keys.push((pk, *stake));
        }
        let aggregate_key: A::CheckedPK = keys.iter().map(|k| &k.0).sum();

        // This ensures the order is the same for permutations of the input keys
        keys.sort();

        let mut tree_vec = Vec::with_capacity(keys_pop.len());
        let mut leaf_map = HashMap::new();
        for (index, (key, stake)) in keys.iter().enumerate() {
            leaf_map.insert(key.clone(), (*stake, index));
            tree_vec.push(MTValue(key.clone(), *stake));
        }

        Ok(Avk {
            aggregate_key,
            tree: MerkleTree::create(&tree_vec),
            leaf_map,
            total_stake,
            threshold,
        })
    }

    /// Check that this aggregation is derived from the given sequence of valid keys.
    /// Called `ACheck` in the paper.
    pub fn check<F>(&self, keys: &[(A::PreCheckedPK, Stake)]) -> Result<(), AtmsError<A, F, H>> {
        // Stake is irrelevant for this check.
        let akey2 = Self::new(keys, 0)?;
        if self.tree.root() == akey2.tree.root() && self.aggregate_key == akey2.aggregate_key {
            return Ok(());
        }
        Err(AtmsError::InvalidAggregation(akey2))
    }
}

impl<A, F> Asig<A, F>
where
    F: Clone,
    A: Atms,
    A::CheckedPK: Clone
        + Eq
        + Hash
        + Sub<A::CheckedPK, Output = A::CheckedPK>
        + for<'a> Sum<&'a A::CheckedPK>
        + std::fmt::Debug
        + Ord,
    A::SIG: for<'a> Sum<&'a A::SIG> + Ord + Clone,
{
    /// Aggregate a list of signatures.
    pub fn new<H: MTHashLeaf<MTValue<A::CheckedPK>, F = F> + Digest>(
        keys: &Avk<A, H>,
        sigs: &[(A::CheckedPK, A::SIG)],
    ) -> Self {
        let signers = sigs.iter().map(|(k, _)| k).collect::<HashSet<_>>();
        let keys_proofs = keys
            .leaf_map
            .keys()
            .filter_map(|k| {
                if !signers.contains(k) {
                    let (_, idx) = keys.leaf_map.get(k)?;
                    Some((k.clone(), keys.tree.get_path(*idx)))
                } else {
                    None
                }
            })
            .collect::<Vec<(_, _)>>();

        // lets get a list of unique signatures. We first sort them and then `dedup` as we don't
        // care of the order of the signatures. However, we do care of having duplicates.
        let mut unique_sigs = sigs.to_vec();
        unique_sigs.sort_unstable();
        unique_sigs.dedup();

        let aggregate: A::SIG = unique_sigs.iter().map(|(_, s)| s).sum();
        Self {
            keys_proofs,
            aggregate,
        }
    }

    /// Verify that this aggregation is valid for the given collection of keys and message.
    /// Called `AVer` in the paper
    pub fn verify<H: MTHashLeaf<MTValue<A::CheckedPK>, F = F> + Digest>(
        &self,
        msg: &[u8],
        keys: &Avk<A, H>,
    ) -> Result<(), AtmsError<A, F, H>> {
        // Check duplicates by building this set of
        // non-signing keys
        let mut unique_non_signers = HashSet::new();
        let mut non_signing_stake = 0;

        // Check inclusion proofs
        for (non_signer, proof) in &self.keys_proofs {
            if let Some((stake, idx)) = keys.leaf_map.get(non_signer) {
                if keys
                    .tree
                    .check(&MTValue(non_signer.clone(), *stake), *idx, proof)
                {
                    non_signing_stake += stake;
                    // Check non-signers are distinct
                    if !unique_non_signers.insert(non_signer) {
                        return Err(AtmsError::FoundDuplicates(non_signer.clone()));
                    }
                } else {
                    return Err(AtmsError::InvalidMerkleProof(
                        *idx,
                        non_signer.clone(),
                        proof.clone(),
                    ));
                }
            } else {
                return Err(AtmsError::UnknownKey(non_signer.clone()));
            }
        }

        if non_signing_stake >= keys.total_stake - keys.threshold {
            return Err(AtmsError::TooMuchOutstandingStake(non_signing_stake));
        }

        // Check with the underlying signature scheme that the quotient of the
        // aggregated key by the non-signers validates this signature.
        let avk2 = keys.aggregate_key.clone() - unique_non_signers.into_iter().sum();
        if !A::verify(msg, &avk2, &self.aggregate) {
            return Err(AtmsError::InvalidSignature(self.aggregate.clone()));
        }

        Ok(())
    }
}

mod msp {
    ///! Instantiate A for the MSP scheme implemented in this crate
    use super::*;
    use crate::msp::{Msp, MspMvk, MspPk, MspSig};
    use ark_ec::PairingEngine;

    impl<PE> Atms for Msp<PE>
    where
        PE: PairingEngine + Hash,
    {
        type PreCheckedPK = MspPk<PE>;
        type CheckedPK = MspMvk<PE>;
        type SIG = MspSig<PE>;

        fn check_key(proof: &Self::PreCheckedPK) -> Option<Self::CheckedPK> {
            if Msp::check(proof) {
                return Some(proof.mvk);
            }
            None
        }

        fn verify(msg: &[u8], pk: &Self::CheckedPK, sig: &Self::SIG) -> bool {
            Msp::aggregate_ver(msg, pk, sig)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::msp::*;
    use ark_bls12_377::Bls12_377;
    use blake2::Blake2b;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    type C = Bls12_377;
    type H = Blake2b;
    type A = Msp<C>;
    type F = <H as MTHashLeaf<MTValue<MspMvk<C>>>>::F;

    proptest! {
    #[test]
    fn test_atms_protocol(n in 5..=32_usize,
                          subset_is in vec(1..=32_usize, 1..=32_usize),
                          threshold_frac in 2..=4_usize,
                          msg in any::<[u8; 16]>(),
                          // seed in any::<[u8; 32]>()
        ) {
            let seed = [0u8; 32];
            let threshold: u64 = (n - n/threshold_frac) as u64;
            let mut rng = ChaCha20Rng::from_seed(seed);

            let mut keys: Vec<(MspPk<C>, Stake)> = Vec::new();
            let mut signatures: Vec<(MspMvk<C>, MspSig<C>)> = Vec::new();
            for _ in 1..=n {
                let (sk, pk) = Msp::<C>::gen(&mut rng);
                let sig = Msp::sig(&sk, &msg);
                assert!(Msp::ver(&msg, &pk.mvk, &sig));
                keys.push((pk, 1));
                signatures.push((pk.mvk, sig));
            }

            let avk = Avk::<A, H>::new::<F>(&keys, threshold);
            assert!(avk.is_ok());
            let avk = avk.unwrap();
            assert!(avk.check::<F>(&keys).is_ok());

            // Note that we accept repeated signatures.
            let subset = subset_is
                .iter()
                .map(|i| {
                    signatures[i % n]
                })
                .collect::<Vec<_>>();

            let aggr_sig = Asig::new(&avk, &subset);

            match aggr_sig.verify(&msg, &avk) {
                Ok(()) => {
                    assert!(subset.len() >= threshold as usize)
                },
                Err(AtmsError::FoundDuplicates(_)) => {
                    assert!(subset.iter().map(|x| x.0).collect::<HashSet<_>>().len() < subset.len())
                }
                Err(AtmsError::TooMuchOutstandingStake(d)) => {
                    assert!(d >= avk.total_stake - threshold);
                }
                Err(err) => unreachable!("{:?}", err)
            }
        }
    }
}
