use std::cmp::Ordering;

use crate::{
    LotteryIndex, MembershipDigest, StmResult, UniqueSchnorrSignature,
    circuits::MerklePath as Halo2MerklePath,
    membership_commitment::{MerkleTree, MerkleTreeSnarkLeaf},
    proof_system::halo2_snark::witness::SignatureRegistrationEntry,
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

    /// Expands a [`SignatureRegistrationEntry`] into one `WitnessEntry` per winning lottery index,
    /// computing the circuit-facing authentication path from the given tree using a pre-resolved
    /// leaf index.
    ///
    /// The caller is expected to resolve the leaf index via
    /// [`MerkleTree::build_leaf_index_map`] for O(1) lookups when processing multiple signatures.
    pub(crate) fn convert_snark_single_signature_to_witness_entries<D: MembershipDigest>(
        signature_registration_entry: SignatureRegistrationEntry,
        merkle_tree: &MerkleTree<D::SnarkHash, MerkleTreeSnarkLeaf>,
        leaf_index: usize,
    ) -> StmResult<Vec<WitnessEntry>> {
        let merkle_tree_leaf = signature_registration_entry.get_registration_entry();
        let merkle_path = merkle_tree.compute_merkle_tree_path(leaf_index);
        let merkle_path_for_circuit: Halo2MerklePath = Halo2MerklePath::try_from(&merkle_path)?;
        let unique_schnorr_signature =
            signature_registration_entry.get_signature().get_schnorr_signature();

        let witness_entries = signature_registration_entry
            .get_signature()
            .get_indices()
            .iter()
            .map(|&lottery_index| {
                WitnessEntry::new(
                    *merkle_tree_leaf,
                    merkle_path_for_circuit.clone(),
                    unique_schnorr_signature,
                    lottery_index,
                )
            })
            .collect();

        Ok(witness_entries)
    }

    pub fn get_lottery_index(&self) -> LotteryIndex {
        self.lottery_index
    }

    pub fn get_unique_schnorr_signature(&self) -> UniqueSchnorrSignature {
        self.unique_schnorr_signature
    }

    pub fn get_merkle_tree_leaf(&self) -> MerkleTreeSnarkLeaf {
        self.merkle_tree_leaf
    }

    pub fn get_merkle_path(&self) -> Halo2MerklePath {
        self.merkle_path.clone()
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
