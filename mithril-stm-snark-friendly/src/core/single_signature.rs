#[cfg(feature = "future_snark")]
use crate::signature_scheme::schnorr_signature::JubjubSignature;
use crate::{core::SignerIndex, signature_scheme::bls_signature::BlsSignature, *};

/// Single signature produced by a signer
pub struct SingleSignature {
    pub signer_index: SignerIndex,
    pub bls_signature: BlsSignature,
    pub concatenation_indices: Vec<u64>,
    #[cfg(feature = "future_snark")]
    pub schnorr_signature: Option<JubjubSignature>,
    #[cfg(feature = "future_snark")]
    pub snark_indices: Option<Vec<u64>>,
}
