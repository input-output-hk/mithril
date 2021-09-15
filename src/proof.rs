use crate::msp;
use crate::merkle_tree::MerkleTree;
use crate::party::Sig;
use super::Index;

#[derive(Clone)]
pub struct Witness {
    pub sigs: Vec<Sig>,
    pub indices: Vec<Index>,
    pub evals: Vec<u64>,
}

// Proof system that simply concatenates the witness
#[derive(Clone)]
pub struct ConcatProof(Witness);

impl ConcatProof {
    pub fn prove(_avk: &MerkleTree, _ivk: &msp::MVK, _msg: &[u8], w: &Witness) -> Self {
        Self(w.clone())
    }

    pub fn verify(&self, avk: &MerkleTree, ivk: &msp::MVK, msg: &[u8]) -> bool {
        unimplemented!()
    }
}
