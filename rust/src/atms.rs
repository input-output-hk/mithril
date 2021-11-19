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
use std::iter::{FromIterator, Sum};
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
/// of possession. We need the ability to verify the Proof of Possesion, and to
/// verify the signature.
pub trait Atms: Sized + Debug {
    /// Proof of Possession of the secret key with respect to the Public Key.
    type POP: Copy;
    /// Associated type that defines the Public Key.
    type PK;
    /// Associated type that defines the Signature.
    type SIG;
    /// Verify the proof of possesion. If it is valid, return the public key (without
    /// the proof of possession)
    fn verify_proof(proof: &Self::POP) -> Option<Self::PK>;
    /// Verify that the signature is valid for a message `msg` and key `key`
    fn verify(msg: &[u8], pk: &Self::PK, sig: &Self::SIG) -> bool;
}

/// An aggregated key, that contains the aggregation of a set of keys, the merkle commitment of
/// this set of keys, a map mapping the position of a key within the merkle tree and the total
/// stake of the system.
// todo: maybe we can implement PartEq and Eq
#[derive(Debug)]
pub struct Avk<A, H>
where
    A: Atms,
    H: MTHashLeaf<MTValue<A::PK>> + Digest,
{
    /// The product of the aggregated keys
    aggregate_key: A::PK,
    /// The Merkle commitment to the set of keys
    tree: MerkleTree<MTValue<A::PK>, H>,
    /// Mapping to identify position of key within merkle tree
    leaf_map: HashMap<A::PK, (Stake, usize)>,
    /// Total stake
    total_stake: Stake,
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
    keys_proofs: Vec<(A::PK, Path<F>)>,
}

#[derive(Debug)]
/// Errors associated with Atms
pub enum AtmsError<A, F>
where
    A: Atms,
{
    /// An unknown key (we don't know its index in the merkle tree)
    UnknownKey(A::PK),
    /// The proof that `PK` is at a given idx is false
    InvalidMerkleProof(usize, A::PK, Path<F>),
    /// The proof that the secret key with respect to `PK` is known, fails
    InvalidPoPProof(A::POP),
    /// Duplicate non-signers in signature
    FoundDuplicates,
    /// Non-signers sum to the given stake, which is more than half of total
    TooMuchOutstandingStake(Stake),
    /// Underlying signature scheme failed to verify
    InvalidSignature,
    /// The given public key does not correspond to the aggregation of the given
    /// set of keys
    InvalidAggregation,
}

impl<A, H> Avk<A, H>
where
    A: Atms,
    A::PK: Eq + Ord + Clone + Hash + for<'a> Sum<&'a A::PK> + ToBytes,
    H: MTHashLeaf<MTValue<A::PK>> + Digest,
{
    /// Aggregate a set of keys, and commit to them in a canonical order.
    /// Called `AKey` in the paper.
    pub fn new<F>(keys_pop: &[(A::POP, Stake)]) -> Result<Self, AtmsError<A, F>> {
        let mut keys: Vec<(A::PK, Stake)> = Vec::with_capacity(keys_pop.len());
        for (key, stake) in keys_pop {
            let pk = match A::verify_proof(key) {
                None => return Err(AtmsError::InvalidPoPProof(*key)),
                Some(p) => p,
            };
            keys.push((pk, *stake))
        }

        // This ensures the order is the same for permutations of the input keys
        keys.sort();

        let aggregate_key: A::PK = keys.iter().map(|k| &k.0).sum();
        let tree = MerkleTree::create(
            keys.iter()
                .map(|(k, s)| MTValue(k.clone(), *s))
                .collect::<Vec<_>>()
                .as_ref(),
        );
        let leaf_map =
            HashMap::from_iter(keys.into_iter().enumerate().map(|(i, (k, s))| (k, (s, i))));
        let total_stake = keys_pop.iter().map(|x| x.1).sum();

        Ok(Avk {
            aggregate_key,
            tree,
            leaf_map,
            total_stake,
        })
    }

    /// Check that this aggregation is derived from the given sequence of valid keys.
    /// Called `ACheck` in the paper.
    pub fn check<F>(&self, keys: &[(A::POP, Stake)]) -> Result<(), AtmsError<A, F>> {
        let akey2 = Self::new(keys)?;
        if self.tree.root() == akey2.tree.root() && self.aggregate_key == akey2.aggregate_key {
            return Ok(());
        }
        Err(AtmsError::InvalidAggregation)
    }
}

impl<A, F> Asig<A, F>
where
    F: Clone,
    A: Atms,
    A::PK:
        Clone + Eq + Hash + Sub<A::PK, Output = A::PK> + for<'a> Sum<&'a A::PK> + std::fmt::Debug,
    A::SIG: for<'a> Sum<&'a A::SIG>,
{
    /// Aggregate a list of signatures.
    pub fn new<H: MTHashLeaf<MTValue<A::PK>, F = F> + Digest>(
        keys: &Avk<A, H>,
        sigs: &[(A::PK, A::SIG)],
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
        let aggregate: A::SIG = sigs.iter().map(|(_, s)| s).sum();
        Self {
            keys_proofs,
            aggregate,
        }
    }

    /// Verify that this aggregation is valid for the given collection of keys and message.
    /// Called `AVer` in the paper
    pub fn verify<H: MTHashLeaf<MTValue<A::PK>, F = F> + Digest>(
        &self,
        msg: &[u8],
        keys: &Avk<A, H>,
    ) -> Result<(), AtmsError<A, F>> {
        // Check duplicates by building this set of
        // non-signing keys
        let mut unique_non_signers = HashSet::new();
        let mut non_signing_stake = 0;

        // Check inclusion proofs
        for (non_signer, proof) in &self.keys_proofs {
            if let Some((stake, idx)) = keys.leaf_map.get(non_signer) {
                if !keys
                    .tree
                    .check(&MTValue(non_signer.clone(), *stake), *idx, proof)
                {
                    return Err(AtmsError::InvalidMerkleProof(
                        *idx,
                        non_signer.clone(),
                        proof.clone(),
                    ));
                } else {
                    non_signing_stake += stake;
                    // Check non-signers are distinct
                    if !unique_non_signers.insert(non_signer) {
                        return Err(AtmsError::UnknownKey(non_signer.clone()));
                    }
                }
            } else {
                return Err(AtmsError::FoundDuplicates);
            }
        }

        if non_signing_stake >= keys.total_stake {
            return Err(AtmsError::TooMuchOutstandingStake(non_signing_stake));
        }

        // Check with the underlying signature scheme that the quotient of the
        // aggregated key by the non-signers validates this signature.
        let avk2 = keys.aggregate_key.clone() - unique_non_signers.into_iter().sum();
        if !A::verify(msg, &avk2, &self.aggregate) {
            return Err(AtmsError::InvalidSignature);
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
        type POP = MspPk<PE>;
        type PK = MspMvk<PE>;
        type SIG = MspSig<PE>;

        fn verify_proof(proof: &Self::POP) -> Option<Self::PK> {
            if Msp::check(proof) {
                return Some(proof.mvk);
            }
            None
        }

        fn verify(msg: &[u8], pk: &Self::PK, sig: &Self::SIG) -> bool {
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
    fn test_atms_protocol(n in 1..=32_usize,
                          subset_is in vec(1..=32_usize, 1..=32_usize),
                          t_frac in 1..=4_usize,
                          // msg in any::<[u8; 16]>(),
                          // seed in any::<[u8; 32]>()
        ) {
            let msg = [0u8; 16];
            let seed = [0u8; 32];
            // todo: what was this meant for?
            let _t = n/t_frac;
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

            let avk = Avk::<A, H>::new::<F>(&keys);
            assert!(avk.is_ok());
            let avk = avk.unwrap();
            assert!(avk.check::<F>(&keys).is_ok());

            let unique_is = subset_is
                .iter()
                .map(|i| i % n)
                .collect::<HashSet<_>>();

            let subset = unique_is
                .iter()
                .map(|i| {
                    signatures[i % n]
                })
                .collect::<Vec<_>>();
            let aggr_sig = Asig::new(&avk, &subset);

            match aggr_sig.verify(&msg, &avk) {
                Ok(()) => (),
                Err(AtmsError::FoundDuplicates) => {
                    assert!(subset.iter().map(|x| x.0).collect::<HashSet<_>>().len() < subset.len())
                }
                Err(AtmsError::TooMuchOutstandingStake(d)) => {
                    assert!(d as usize > n/2);
                }
                Err(err) => unreachable!("{:?}", err)
            }
        }
    }
}
