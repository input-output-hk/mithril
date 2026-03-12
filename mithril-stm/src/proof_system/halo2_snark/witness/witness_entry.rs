use std::cmp::Ordering;

use crate::{
    LotteryIndex, UniqueSchnorrSignature, circuits::MerklePath as Halo2MerklePath,
    membership_commitment::MerkleTreeSnarkLeaf,
};

/// Per-winning-lottery-index witness data for the SNARK circuit.
///
/// Each entry pairs the signer's Merkle membership proof with the Schnorr signature and lottery
/// index that won the eligibility check.
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
    /// Creates a `WitnessEntry` from its components.
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
}

impl PartialOrd for WitnessEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WitnessEntry {
    fn cmp(&self, other: &Self) -> Ordering {
        self.lottery_index.cmp(&other.lottery_index)
    }
}
