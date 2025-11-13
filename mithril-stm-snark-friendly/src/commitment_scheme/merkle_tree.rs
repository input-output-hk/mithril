use std::marker::PhantomData;

use crate::{
    commitment_scheme::interface::{
        SignerRegistrationCommitmentGenerator, SignerRegistrationRegisterer,
        SignerRegistrationRevealProver, SignerRegistrationRevealVerifier,
    },
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
pub struct MerklePath {
    pub root: Vec<u8>,
}

impl MerklePath {
    pub fn verify(&self) -> StdResult<()> {
        Ok(())
    }
}

/// Merkle Tree structure
pub struct MerkleTree<D: Digest> {
    nodes: Vec<Vec<u8>>,
    hasher: PhantomData<D>,
}

impl<D: Digest> MerkleTree<D> {
    pub fn new<L: MerkleTreeLeaf>(leaves: &[L]) -> MerkleTree<D> {
        let leaves = leaves
            .iter()
            .map(|leaf| D::digest(&leaf.to_bytes()).to_vec())
            .collect::<Vec<_>>();
        let nodes = vec![];

        Self {
            nodes,
            hasher: PhantomData,
        }
    }

    pub fn to_merkle_tree_commitment(&self) -> StdResult<MerkleTreeCommitment> {
        Ok(MerkleTreeCommitment { root: vec![] })
    }

    pub fn compute_merkle_path(&self, _index: usize) -> StdResult<MerklePath> {
        Ok(MerklePath { root: vec![] })
    }
}

impl<D: Digest> SignerRegistrationRegisterer for MerkleTree<D> {
    type RegistrationInput = usize;
    type RegistrationIndex = usize;

    fn register_entry(
        &self,
        reveal: &Self::RegistrationInput,
    ) -> StdResult<Self::RegistrationIndex> {
        Ok(*reveal)
    }
}

impl<D: Digest> SignerRegistrationCommitmentGenerator for MerkleTree<D> {
    type SignerCommitment = MerkleTreeCommitment;

    fn create_signer_registration_commitment(&self) -> StdResult<Self::SignerCommitment> {
        self.to_merkle_tree_commitment()
    }
}

impl<D: Digest> SignerRegistrationRevealProver for MerkleTree<D> {
    type RevealInput = usize;
    type RevealProof = MerklePath;

    fn create_reveal_proof(&self, reveal: &Self::RevealInput) -> StdResult<Self::RevealProof> {
        self.compute_merkle_path(*reveal)
    }
}

impl<D: Digest> SignerRegistrationRevealVerifier for MerkleTree<D> {
    type RevealProof = MerklePath;

    fn verify_reveal_proof(&self, proof: &Self::RevealProof) -> StdResult<()> {
        proof.verify()
    }
}
