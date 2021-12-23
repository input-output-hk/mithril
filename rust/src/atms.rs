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
    /// Public key after preparation for ATMS protocol. This preparation may be a verification
    /// of a proof of correctness or a computation of an associated public exponent.
    type PreparedPk;
    /// Associated type that defines the Signature.
    type SIG;
    /// Prepare a set of public keys. If all are valid, return a vector formed of tuples of
    /// public keys and their corresponding stake, as well as the total stake.
    fn prepare_keys(
        keys_pop: &[(Self::PreCheckedPK, Stake)],
    ) -> Option<(Vec<(Self::PreparedPk, Stake)>, Stake)>;
    /// Verify that the signature is valid for a message `msg` and key `key`
    fn verify(msg: &[u8], pk: &Self::PreparedPk, sig: &Self::SIG) -> bool;
}

/// An aggregated key, that contains the aggregation of a set of keys, the merkle commitment of
/// this set of keys, a map mapping the position of a key within the merkle tree and the total
/// stake of the system.
#[derive(Debug)]
pub struct Avk<A, H>
where
    A: Atms,
    H: MTHashLeaf<MTValue<A::PreparedPk>> + Digest,
{
    /// The product of the aggregated keys
    aggregate_key: A::PreparedPk,
    /// The Merkle commitment to the set of keys
    tree: MerkleTree<MTValue<A::PreparedPk>, H>,
    /// Mapping to identify position of key within merkle tree
    leaf_map: HashMap<A::PreparedPk, (Stake, usize)>,
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
    keys_proofs: Vec<(A::PreparedPk, Path<F>)>,
}

#[derive(Debug, thiserror::Error)]
/// Errors associated with Atms
pub enum AtmsError<A, H>
where
    A: Atms,
    H: MTHashLeaf<MTValue<A::PreparedPk>> + Digest,
{
    /// An unknown key (we don't know its index in the merkle tree)
    #[error("Key not present in the Merkle Tree.")]
    UnknownKey(A::PreparedPk),
    /// The proof that `PK` is at a given idx is false
    #[error("Proof that key with index {0} is in Merkle Tree is invalid.")]
    InvalidMerkleProof(usize, A::PreparedPk, Path<H::F>),
    /// A key submitted for aggregation is invalid.
    #[error("Invalid key provided in the set of keys.")]
    InvalidKey,
    /// Duplicate non-signers in signature
    #[error("Submitted keys of non-signers contains duplicates.")]
    FoundDuplicates(A::PreparedPk),
    /// Non-signers sum to the given stake, which is more than half of total
    #[error("Signatures do not exceed the required threshold {0}.")]
    TooMuchOutstandingStake(Stake),
    /// Underlying signature scheme failed to verify
    #[error("Invalid Signature.")]
    InvalidSignature(A::SIG),
    /// The given public key does not correspond to the aggregation of the given
    /// set of keys
    #[error("Given public key does not correspond to the aggregation of the set of public keys.")]
    InvalidAggregation(Avk<A, H>),
}

impl<A, H> Avk<A, H>
where
    A: Atms,
    A::PreparedPk: Eq + Ord + Clone + Hash + for<'a> Sum<&'a A::PreparedPk> + ToBytes,
    H: MTHashLeaf<MTValue<A::PreparedPk>> + Digest,
{
    /// Aggregate a set of keys, and commit to them in a canonical order.
    /// Called `AKey` in the paper.
    pub fn new(
        keys_pop: &[(A::PreCheckedPK, Stake)],
        threshold: Stake,
    ) -> Result<Self, AtmsError<A, H>> {
        let (mut keys, total_stake): (Vec<(A::PreparedPk, Stake)>, Stake) =
            match A::prepare_keys(keys_pop) {
                None => return Err(AtmsError::InvalidKey),
                Some(a) => a,
            };

        let aggregate_key: A::PreparedPk = keys.iter().map(|k| &k.0).sum();

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
    pub fn check(&self, keys: &[(A::PreCheckedPK, Stake)]) -> Result<(), AtmsError<A, H>> {
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
    A::PreparedPk: Clone
        + Eq
        + Hash
        + Sub<A::PreparedPk, Output = A::PreparedPk>
        + for<'a> Sum<&'a A::PreparedPk>
        + std::fmt::Debug
        + Ord,
    A::SIG: for<'a> Sum<&'a A::SIG> + Ord + Clone,
{
    /// Aggregate a list of signatures.
    pub fn new<H: MTHashLeaf<MTValue<A::PreparedPk>, F = F> + Digest>(
        keys: &Avk<A, H>,
        sigs: &[(A::PreparedPk, A::SIG)],
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
    pub fn verify<H: MTHashLeaf<MTValue<A::PreparedPk>, F = F> + Digest>(
        &self,
        msg: &[u8],
        keys: &Avk<A, H>,
    ) -> Result<(), AtmsError<A, H>> {
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

        if non_signing_stake > keys.total_stake - keys.threshold {
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
        type PreparedPk = MspMvk<PE>;
        type SIG = MspSig<PE>;

        fn prepare_keys(
            keys_pop: &[(Self::PreCheckedPK, Stake)],
        ) -> Option<(Vec<(Self::PreparedPk, Stake)>, Stake)> {
            let mut keys: Vec<(Self::PreparedPk, Stake)> = Vec::with_capacity(keys_pop.len());
            let mut total_stake: Stake = 0;
            for (key, stake) in keys_pop {
                if Msp::check(key) {
                    keys.push((key.mvk, *stake));
                } else {
                    return None;
                }
                total_stake += stake;
            }

            Some((keys, total_stake))
        }

        fn verify(msg: &[u8], pk: &Self::PreparedPk, sig: &Self::SIG) -> bool {
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

            let avk = Avk::<A, H>::new(&keys, threshold);
            assert!(avk.is_ok());
            let avk = avk.unwrap();
            assert!(avk.check(&keys).is_ok());

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
