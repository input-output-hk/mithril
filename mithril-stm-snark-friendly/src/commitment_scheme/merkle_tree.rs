use std::marker::PhantomData;

use crate::{
    commitment_scheme::{
        SignerRegistrationCommitmentGenerator, SignerRegistrationRegisterer,
        SignerRegistrationRevealProver, SignerRegistrationRevealVerifier,
    },
    core::Digest,
    *,
};

/// Trait representing a leaf in the Merkle Tree
pub trait MerkleTreeLeaf {
    /// Convert the leaf data to bytes
    fn to_bytes(&self) -> Vec<u8>;
}

/// Merkle Tree Commitment structure
pub struct MerkleTreeCommitment {
    pub root: Vec<u8>,
}

/// Merkle Path structure
pub struct MerklePath<D: Digest> {
    pub values: Vec<Vec<u8>>,
    pub index: usize,
    hasher: PhantomData<D>,
}

impl<D: Digest> MerklePath<D> {
    pub fn verify(&self) -> StdResult<()> {
        Ok(())
    }
}

/// Merkle Tree structure
pub struct MerkleTree<D: Digest, L: MerkleTreeLeaf> {
    nodes: Vec<Vec<u8>>,
    hasher: PhantomData<D>,
    leaves: PhantomData<L>,
}

impl<D: Digest, L: MerkleTreeLeaf> MerkleTree<D, L> {
    /// Creates a new Merkle Tree from the given leaves
    pub fn new(leaves: &[L]) -> MerkleTree<D, L> {
        let leaves = leaves
            .iter()
            .map(|leaf| D::digest(&leaf.to_bytes()).to_vec())
            .collect::<Vec<_>>();
        let nodes = vec![];

        Self {
            nodes,
            hasher: PhantomData,
            leaves: PhantomData,
        }
    }

    /// Converts the Merkle Tree to a Merkle Tree Commitment
    pub fn to_merkle_tree_commitment(&self) -> StdResult<MerkleTreeCommitment> {
        Ok(MerkleTreeCommitment { root: vec![] })
    }

    /// Computes the Merkle Path for a given index
    pub fn compute_merkle_path(&self, _index: usize) -> StdResult<MerklePath<D>> {
        Ok(MerklePath {
            values: vec![],
            index: 0,
            hasher: PhantomData,
        })
    }
}

impl<D: Digest, L: MerkleTreeLeaf> SignerRegistrationRegisterer for MerkleTree<D, L> {
    type RegistrationInput = usize;
    type RegistrationIndex = usize;

    fn register_entry(
        &self,
        reveal: &Self::RegistrationInput,
    ) -> StdResult<Self::RegistrationIndex> {
        Ok(*reveal)
    }
}

impl<D: Digest, L: MerkleTreeLeaf> SignerRegistrationCommitmentGenerator for MerkleTree<D, L> {
    type SignerCommitment = MerkleTreeCommitment;

    fn create_signer_registration_commitment(&self) -> StdResult<Self::SignerCommitment> {
        self.to_merkle_tree_commitment()
    }
}

impl<D: Digest, L: MerkleTreeLeaf> SignerRegistrationRevealProver for MerkleTree<D, L> {
    type RevealInput = usize;
    type RevealProof = MerklePath<D>;

    fn create_reveal_proof(&self, reveal: &Self::RevealInput) -> StdResult<Self::RevealProof> {
        self.compute_merkle_path(*reveal)
    }
}

impl<D: Digest, L: MerkleTreeLeaf> SignerRegistrationRevealVerifier for MerkleTree<D, L> {
    type RevealProof = MerklePath<D>;

    fn verify_reveal_proof(&self, proof: &Self::RevealProof) -> StdResult<()> {
        proof.verify()
    }
}
