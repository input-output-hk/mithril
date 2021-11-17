//! Ad-Hoc Threshold Multisignatures
//!
//! Proof-of-Stake Sidechains
//! <https://eprint.iacr.org/2018/1239.pdf>
//!
//! The implementation in this module is parameterized by the underlying
//! signature scheme, which will define its own type of key and signature. All
//! we need here is to be able to verify signatures against keys.

use crate::{merkle_tree::*, stm::Stake};
use ark_ff::ToBytes;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::io::Write;
use std::iter::{FromIterator, Sum};
use std::ops::Sub;

/// The values that are represented in the Merkle Tree.
#[derive(Debug, Clone, Copy)]
pub struct MTValue<VK>(pub VK, pub Stake);

impl<VK: ToBytes> ToBytes for MTValue<VK> {
    fn write<W: Write>(&self, mut writer: W) -> std::result::Result<(), std::io::Error> {
        self.0.write(&mut writer)?;
        self.1.write(&mut writer)
    }
}

/// The implementation relies on an underlying signature scheme
/// All that we need from it is the ability to verify that a signature
/// is valid for a given aggregation of keys
pub trait VerifySig {
    type VK;
    /// Verify that this is a valid signature for message `msg` and keys
    /// aggregated in `key`
    fn verify(&self, msg: &[u8], key: &Self::VK) -> bool;
}

/// Aggregation of Keys
#[derive(Debug)]
pub struct Avk<VK, H>
where
    H: MTHashLeaf<MTValue<VK>>,
{
    /// The product of the aggregated keys
    aggregate_key: VK,
    /// The Merkle commitment to the set of keys
    tree: MerkleTree<MTValue<VK>, H>,
    /// Mapping to identify position of key within merkle tree
    leaf_map: HashMap<VK, (Stake, usize)>,
    /// Total stake
    total_stake: Stake,
}

/// An Aggregated Signature
#[derive(Debug)]
pub struct Asig<VK, Sig, F> {
    /// The product of the aggregated signatures
    aggregate: Sig,
    /// Proofs of membership of non-signing keys in the original
    /// set of keys
    keys_proofs: Vec<(VK, Path<F>)>,
}

#[derive(Debug)]
pub enum VerifyFailure<VK, F> {
    /// An unknown key (we don't know its index in the merkle tree)
    UnknownKey(VK),
    /// The proof that VK is at a given idx is false
    InvalidProof(usize, VK, Path<F>),
    /// Duplicate non-signers in signature
    FoundDuplicates,
    /// Non-signers sum to the given stake, which is more than half of total
    TooMuchOutstandingStake(Stake),
    /// Underlying signature scheme failed to verify
    InvalidSignature,
}

impl<VK, H> Avk<VK, H>
where
    VK: Eq + Ord + Clone + Hash + for<'a> Sum<&'a VK> + ToBytes,
    H: MTHashLeaf<MTValue<VK>>,
{
    /// Aggregate a set of keys into a canonical order.
    /// Called `AKey` in the paper.
    pub fn new(keys: &[(VK, Stake)]) -> Self {
        let mut keys2 = keys.to_vec();

        // This ensures the order is the same for permutations of the input keys
        keys2.sort();

        let aggregate_key = keys2.iter().map(|k| &k.0).sum();
        let tree = MerkleTree::create(
            keys2
                .iter()
                .map(|(k, s)| MTValue(k.clone(), *s))
                .collect::<Vec<_>>()
                .as_ref(),
        );
        let leaf_map =
            HashMap::from_iter(keys2.into_iter().enumerate().map(|(i, (k, s))| (k, (s, i))));
        let total_stake = keys.iter().map(|x| x.1).sum();

        Avk {
            aggregate_key,
            tree,
            leaf_map,
            total_stake,
        }
    }

    /// Check that this aggregation is derived from the given sequence of keys.
    /// Called `ACheck` in the paper.
    pub fn check(&self, keys: &[(VK, Stake)]) -> bool {
        let akey2 = Self::new(keys);
        self.tree.root() == akey2.tree.root() && self.aggregate_key == akey2.aggregate_key
    }
}

impl<VK, Sig, F> Asig<VK, Sig, F>
where
    F: Clone,
    VK: Clone + Eq + Hash + Sub<VK, Output = VK> + for<'a> Sum<&'a VK> + std::fmt::Debug,
    Sig: for<'a> Sum<&'a Sig> + for<'a> VerifySig<VK = VK>,
{
    /// Aggregate a sequence of signatures. Called `ASig` in the paper
    pub fn new<H: MTHashLeaf<MTValue<VK>, F = F>>(
        msg: &[u8],
        keys: &Avk<VK, H>,
        sigs: &[(VK, Sig)],
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
        let aggregate: Sig = sigs.iter().map(|(_, s)| s).sum();
        Self {
            keys_proofs,
            aggregate,
        }
    }

    /// Verify that this aggregation is valid for the given collection of keys and message.
    /// Called `AVer` in the paper
    pub fn verify<H: MTHashLeaf<MTValue<VK>, F = F>>(
        &self,
        msg: &[u8],
        keys: &Avk<VK, H>,
    ) -> Result<(), VerifyFailure<VK, F>> {
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
                    return Err(VerifyFailure::InvalidProof(
                        *idx,
                        non_signer.clone(),
                        proof.clone(),
                    ));
                } else {
                    non_signing_stake += stake;
                    unique_non_signers.insert(non_signer);
                }
            } else {
                return Err(VerifyFailure::UnknownKey(non_signer.clone()));
            }
        }

        // Check non-signers are distinct
        if unique_non_signers.len() != self.keys_proofs.len() {
            return Err(VerifyFailure::FoundDuplicates);
        }

        if non_signing_stake >= keys.total_stake {
            return Err(VerifyFailure::TooMuchOutstandingStake(non_signing_stake));
        }

        // Check with the underlying signature scheme that the quotient of the
        // aggregated key by the non-signers validates this signature.
        let avk2 = keys.aggregate_key.clone() - unique_non_signers.into_iter().sum();
        if !self.aggregate.verify(msg, &avk2) {
            return Err(VerifyFailure::InvalidSignature);
        }

        Ok(())
    }
}

mod msp {
    ///! Instantiate ATMS for the MSP scheme implemented in this crate
    use super::*;
    use crate::msp::{Msp, MspMvk, MspSig};
    use ark_ec::PairingEngine;

    impl<PE> VerifySig for MspSig<PE>
    where
        PE: PairingEngine,
    {
        type VK = MspMvk<PE>;

        fn verify(&self, msg: &[u8], key: &Self::VK) -> bool {
            Msp::aggregate_ver(msg, key, self)
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
    type VK = MspMvk<C>;
    type F = <H as MTHashLeaf<MTValue<VK>>>::F;

    proptest! {
    #[test]
    fn test_atms_protocol(n in 1..=32_usize,
                          subset_is in vec(1..=32_usize, 1..=32_usize),
                          t_frac in 1..=4_usize,
                          msg in any::<[u8; 16]>(),
                          seed in any::<[u8; 32]>()) {
        let t = n/t_frac;
        let mut rng = ChaCha20Rng::from_seed(seed);

        let mut signatures = Vec::new();
        for _ in 1..=n {
            let (sk, pk) = Msp::<C>::gen(&mut rng);
            let sig = Msp::sig(&sk, &msg);
            assert!(Msp::ver(&msg, &pk.mvk, &sig));
            signatures.push((pk.mvk, sig));
        }

        let keys = signatures.iter().map(|(k, _)| (*k, 1)).collect::<Vec<(VK, Stake)>>();
        let avk = Avk::<VK, H>::new(&keys);
        assert!(avk.check(&keys));

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
        let aggr_sig = Asig::new(&msg, &avk, &subset);

        match aggr_sig.verify(&msg, &avk) {
            Ok(()) => (),
            Err(VerifyFailure::FoundDuplicates) => {
                assert!(subset.iter().map(|x| x.0).collect::<HashSet<_>>().len() < subset.len())
            }
            Err(VerifyFailure::TooMuchOutstandingStake(d)) => {
                assert!(d as usize > n/2);
            }
            Err(err) => unreachable!("{:?}", err)
        }
    }
    }
}
