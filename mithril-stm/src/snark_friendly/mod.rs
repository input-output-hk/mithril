#![allow(dead_code)]

mod aggregator;
mod bls;
mod concatenation_proof;
mod initializer;
mod interface;
mod key_registration;
mod merkle_tree;
mod pedersen_commitment;
mod schnorr;
mod signer;
mod signer_registration;
mod single_signature;
mod snark_proof;

pub use aggregator::*;
pub use bls::*;
pub use concatenation_proof::*;
pub use initializer::*;
pub use interface::*;
pub use key_registration::*;
pub use merkle_tree::*;
pub use pedersen_commitment::*;
pub use schnorr::*;
pub use signer::*;
pub use signer_registration::*;
pub use single_signature::*;
pub use snark_proof::*;

pub type StdResult<T> = anyhow::Result<T>;

pub struct Parameters {}

#[derive(PartialEq, Eq, Clone)]
pub struct Stake {}

#[derive(Clone)]
pub struct SignerIndex {}

impl From<usize> for SignerIndex {
    fn from(value: usize) -> Self {
        SignerIndex {}
    }
}
