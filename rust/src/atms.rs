//! Ad-Hoc Threshold Multisignatures
//!
//! Proof-of-Stake Sidechains
//! <https://eprint.iacr.org/2018/1239.pdf>
//!
//! The implementation in this module is parameterized by the underlying
//! signature scheme, which will define its own type of key and signature. All
//! we need here is to be able to verify signatures against keys.

use crate::merkle_tree::*;
use crate::Path;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::iter::{FromIterator, Sum};
use std::ops::Sub;

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
pub struct Avk<VK, H>
where
    H: MTHashLeaf<VK>,
{
    /// The product of the aggregated keys
    aggregate_key: VK,
    /// The Merkle commitment to the set of keys
    tree: MerkleTree<VK, H>,
    /// Mapping to identify position of key within merkle tree
    leaf_map: HashMap<VK, usize>,
}

/// An Aggregated Signature
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
    /// Found `x` signatures which is less than the threshold
    InsufficientSignatures(usize),
    /// Underlying signature scheme failed to verify
    InvalidSignature,
}

impl<VK, H> Avk<VK, H>
where
    VK: Eq + Ord + Clone + Hash + for<'a> Sum<&'a VK>,
    H: MTHashLeaf<VK>,
{
    /// Aggregate a set of keys into a canonical order.
    /// Called `AKey` in the paper.
    pub fn new(keys: &[VK]) -> Self {
        let mut keys2 = keys.to_vec();

        // This ensures the order is the same for permutations of the input keys
        keys2.sort();

        let aggregate_key = keys2.iter().sum();
        let tree = MerkleTree::create(keys2.as_ref());
        let leaf_map = HashMap::from_iter(keys2.into_iter().enumerate().map(|(x, y)| (y, x)));

        Avk {
            aggregate_key,
            tree,
            leaf_map,
        }
    }

    /// Check that this aggregation is derived from the given sequence of keys.
    /// Called `ACheck` in the paper.
    pub fn check(&self, keys: &[VK]) -> bool {
        let akey2 = Self::new(keys);
        self.tree.root() == akey2.tree.root() && self.aggregate_key == akey2.aggregate_key
    }
}

impl<VK, Sig, F> Asig<VK, Sig, F>
where
    F: Clone,
    VK: Clone + Eq + Hash + Sub<VK, Output = VK> + for<'a> Sum<&'a VK>,
    Sig: for<'a> Sum<&'a Sig> + for<'a> VerifySig<VK = VK>,
{
    /// Aggregate a sequence of signatures. Called `ASig` in the paper
    pub fn new<H: MTHashLeaf<VK, F = F>>(msg: &[u8], keys: &Avk<VK, H>, sigs: &[(VK, Sig)]) -> Self {
        let signers = sigs.iter().map(|(k, _)| k).collect::<HashSet<_>>();
        let keys_proofs = keys
            .leaf_map
            .keys()
            .filter_map(|k| {
                if !signers.contains(k) {
                    let idx = keys.leaf_map.get(k)?;
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
    pub fn verify<H: MTHashLeaf<VK, F = F>>(
        &self,
        t: usize,
        msg: &[u8],
        keys: &Avk<VK, H>,
    ) -> Result<(), VerifyFailure<VK, F>> {
        // Check duplicates by building this set of
        // non-signing keys
        let mut unique_non_signers = HashSet::new();

        // Check inclusion proofs
        for (non_signer, proof) in &self.keys_proofs {
            if let Some(idx) = keys.leaf_map.get(non_signer) {
                if !keys.tree.check(non_signer, *idx, proof) {
                    return Err(VerifyFailure::InvalidProof(
                        *idx,
                        non_signer.clone(),
                        proof.clone(),
                    ));
                } else {
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

        // Check enough signatures
        let d = keys.leaf_map.keys().len() - unique_non_signers.len();
        if d < t {
            return Err(VerifyFailure::InsufficientSignatures(d));
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
            Msp::ver(msg, key, self)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::msp::*;
    use ark_bls12_377::Bls12_377;
    use blake2::Blake2b;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng};

    type C = Bls12_377;
    type H = Blake2b;
    type VK = MspMvk<C>;
    type F = <H as MTHashLeaf<VK>>::F;

    #[test]
    fn test_atms_protocol() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let mut msg = [0u8; 16];
        rng.fill_bytes(&mut msg);

        let mut signatures = Vec::new();
        for _ in 1..=8 {
            let (sk, pk) = Msp::<C>::gen(&mut rng);
            let sig = Msp::sig(&sk, &msg);
            signatures.push((pk.mvk, sig));
        }

        // Just for fun, "shuffle" the order
        signatures.reverse();

        let keys = signatures.iter().map(|(k, _)| *k).collect::<Vec<VK>>();
        let avk = Avk::<VK, H>::new(&keys);
        assert!(avk.check(&keys));

        let subset = signatures
            .iter()
            .enumerate()
            .filter_map(|(i, x)| if i % 2 == 0 { Some(*x) } else { None })
            .collect::<Vec<_>>();
        let aggr_sig = Asig::new(&msg, &avk, &subset);

        assert!(aggr_sig.verify(3, &msg, &avk).is_ok());
        assert!(aggr_sig.verify(8, &msg, &avk).is_err());
    }
}
