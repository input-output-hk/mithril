use std::collections::{BTreeMap, btree_map::Entry};

use crate::{
    AggregationError, LotteryIndex, MembershipDigest, SignerIndex, SingleSignature, StmResult,
    UniqueSchnorrSignature,
    circuits::MerklePath as Halo2MerklePath,
    membership_commitment::{MerkleTree, MerkleTreeSnarkLeaf},
    proof_system::SnarkClerk,
};

/// Witness data for a single winning lottery index in the SNARK circuit.
///
/// Each entry bundles everything the circuit needs to verify one eligible signature: the signer's
/// Merkle leaf and authentication path (proving membership in the registered set), the Schnorr
/// signature, and the winning lottery index.
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct WitnessEntry {
    /// Signer's registration entry (verification key + lottery target value).
    merkle_tree_leaf: MerkleTreeSnarkLeaf,
    /// Circuit-facing authentication path from the leaf to the Merkle root.
    merkle_path: Halo2MerklePath,
    /// Schnorr signature for this winning lottery index.
    unique_schnorr_signature: UniqueSchnorrSignature,
    /// Winning lottery index.
    lottery_index: LotteryIndex,
}

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl WitnessEntry {
    /// Create a `WitnessEntry` for given input.
    pub(crate) fn new(
        merkle_tree_leaf: MerkleTreeSnarkLeaf,
        merkle_path: Halo2MerklePath,
        unique_schnorr_signature: UniqueSchnorrSignature,
        lottery_index: LotteryIndex,
    ) -> Self {
        Self {
            merkle_tree_leaf,
            merkle_path,
            unique_schnorr_signature,
            lottery_index,
        }
    }

    /// Build the witness vector from a deduplicated map of winning lottery indices.
    ///
    /// The construction proceeds in two phases:
    /// 1. **Signer deduplication** iterates over the signatures to collect, for each unique signer,
    ///    the Merkle leaf (verification key + lottery target) and authentication path. A `BTreeMap`
    ///    keyed by `SignerIndex` ensures each signer's path is computed only once. Signers whose
    ///    registration entry is missing (`Ok(None)`) or whose lookup fails (`Err`) are skipped,
    ///    since the input signatures have already been validated upstream.
    /// 2. **Entry assembly** maps every `(lottery_index, signature)` pair to a `WitnessEntry` by
    ///    looking up the precomputed leaf and path for the signature's signer.
    ///
    /// The returned vector is sorted by lottery index (guaranteed by `BTreeMap` iteration order of
    /// `unique_index_signature_map`).
    ///
    /// Returns an error if a Merkle path cannot be converted to the circuit representation, or
    /// if any witness entry cannot be assembled (missing signer data or SNARK signature).
    pub(crate) fn create_witness<D: MembershipDigest>(
        unique_index_signature_map: BTreeMap<LotteryIndex, SingleSignature>,
        clerk: &SnarkClerk,
    ) -> StmResult<Vec<WitnessEntry>> {
        let merkle_tree: MerkleTree<D::SnarkHash, MerkleTreeSnarkLeaf> =
            clerk.closed_key_registration.to_merkle_tree();
        let mut unique_signers: BTreeMap<SignerIndex, (MerkleTreeSnarkLeaf, Halo2MerklePath)> =
            BTreeMap::new();

        for sig in unique_index_signature_map.values() {
            if let Entry::Vacant(vacant) = unique_signers.entry(sig.signer_index) {
                let leaf = match clerk.get_snark_registration_entry(sig.signer_index) {
                    Ok(Some(entry)) => entry,
                    Ok(None) => continue,
                    Err(_) => continue,
                };
                let merkle_path = merkle_tree.compute_merkle_tree_path(sig.signer_index as usize);
                let merkle_path_circuit: Halo2MerklePath = Halo2MerklePath::try_from(&merkle_path)?;
                vacant.insert((leaf, merkle_path_circuit));
            }
        }

        unique_index_signature_map
            .into_iter()
            .map(|(lottery_index, sig)| {
                let (leaf, merkle_path) = unique_signers
                    .get(&sig.signer_index)
                    .ok_or(AggregationError::MissingSnarkSignerData(sig.signer_index))?;
                let schnorr_sig = sig
                    .snark_signature
                    .as_ref()
                    .ok_or(AggregationError::MissingSnarkSignature(lottery_index))?
                    .get_schnorr_signature();
                Ok(WitnessEntry::new(
                    *leaf,
                    merkle_path.clone(),
                    schnorr_sig,
                    lottery_index,
                ))
            })
            .collect()
    }
}
