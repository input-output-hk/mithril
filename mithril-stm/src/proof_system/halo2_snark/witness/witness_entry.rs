use crate::{
    LotteryIndex, MembershipDigest, UniqueSchnorrSignature,
    membership_commitment::{MerklePath, MerkleTreeSnarkLeaf},
};

/// Per-winning-lottery-index witness data for the SNARK proof.
///
/// Each `WitnessEntry` corresponds to a single winning lottery index and contains
/// the signature along with the signer's Merkle tree leaf and path for membership
/// proof inside the SNARK circuit.
// TODO: add conversion function(s) to comply with the expected circuit input format
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
pub(crate) struct WitnessEntry<D: MembershipDigest> {
    /// The signer's registration entry as a Merkle tree leaf
    merkle_tree_leaf: MerkleTreeSnarkLeaf,
    /// The Merkle path from the leaf to the root,
    merkle_path: MerklePath<D::SnarkHash>,
    /// The Schnorr signature corresponding to the winning lottery index
    unique_schnorr_signature: UniqueSchnorrSignature,
    /// The winning lottery index
    lottery_index: LotteryIndex,
}

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl<D: MembershipDigest> WitnessEntry<D> {
    /// Create a new `WitnessEntry` from its components.
    pub(crate) fn new(
        merkle_tree_leaf: MerkleTreeSnarkLeaf,
        merkle_path: MerklePath<D::SnarkHash>,
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
