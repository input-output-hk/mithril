use crate::{
    LotteryIndex, MembershipDigest, UniqueSchnorrSignature,
    membership_commitment::{MerklePath, MerkleTreeSnarkLeaf},
};

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
pub(crate) struct SignerWitness<D: MembershipDigest> {
    merkle_tree_leaf: MerkleTreeSnarkLeaf,
    merkle_path: MerklePath<D::SnarkHash>,
    unique_schnorr_signature: UniqueSchnorrSignature,
    lottery_index: LotteryIndex,
}

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl<D: MembershipDigest> SignerWitness<D> {
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
