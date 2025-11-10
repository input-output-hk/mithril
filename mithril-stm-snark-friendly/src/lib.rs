#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

mod aggregator;
mod commitment_scheme;
mod initializer;
mod key_registration;
mod proof_system;
mod signature_scheme;
mod signer;
mod single_signature;

pub use aggregator::*;
pub use commitment_scheme::merkle_tree::*;
pub use commitment_scheme::pedersen_commitment::*;
pub use initializer::*;
pub use key_registration::*;
pub use proof_system::concatenation_proof::*;
pub use proof_system::interface::*;
pub use proof_system::snark_proof::*;
pub use signature_scheme::bls_signature::*;
pub use signature_scheme::schnorr_signature::*;
pub use signer::*;
pub use single_signature::*;

pub type StdResult<T> = anyhow::Result<T>;

#[derive(Clone)]
pub struct Parameters {}

#[derive(PartialEq, Eq, Clone)]
pub struct Stake {}

#[derive(Clone)]
pub struct SignerIndex {}

impl From<usize> for SignerIndex {
    fn from(_value: usize) -> Self {
        SignerIndex {}
    }
}

pub trait Digest {
    fn digest(data: &[u8]) -> Vec<u8>;
}
