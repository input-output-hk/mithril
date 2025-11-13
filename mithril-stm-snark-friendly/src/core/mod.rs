pub mod aggregator;
pub mod eligibility;
pub mod initializer;
pub mod key_registration;
pub mod signer;
pub mod single_signature;

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
