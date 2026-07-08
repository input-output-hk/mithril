//! Constraint gadgets used by the recursive IVC circuit.

mod schnorr_signature;

pub(crate) use schnorr_signature::{GenesisSchnorrSignatureInputs, verify_genesis_signature};
