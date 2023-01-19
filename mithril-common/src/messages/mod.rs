//! Messages module
//! This module aims at providing shared structures for API communications.
mod register_signature;
mod register_signer;

pub use register_signature::RegisterSignatureMessage;
pub use register_signer::RegisterSignerMessage;
