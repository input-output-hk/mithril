//! Constraint gadgets used by the recursive IVC circuit.

mod byte_combination;
mod schnorr_signature;

pub(crate) use byte_combination::combine_bytes;
pub(crate) use schnorr_signature::{GenesisSchnorrSignatureInputs, verify_genesis_signature};
