use crate::{
    LotteryIndex, MembershipDigest, UniqueSchnorrSignature,
    membership_commitment::{MerklePath, MerkleTreeSnarkLeaf},
};

#[allow(dead_code)]
pub(crate) struct SignerWitness<D: MembershipDigest> {
    merkle_tree_leaf: MerkleTreeSnarkLeaf,
    merkle_path: MerklePath<D::SnarkHash>,
    unique_schnorr_signature: UniqueSchnorrSignature,
    lottery_index: LotteryIndex,
}

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
