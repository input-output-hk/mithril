//! Circuit boundary types for the Halo2 IVC circuit.
//!
//! This module gives domain names to values that are represented as raw field
//! elements or bytes at lower circuit/gadget boundaries.

#![allow(dead_code)]

use ff::Field;

use super::{
    F, PREIMAGE_CURRENT_EPOCH_BYTES, PREIMAGE_NEXT_MERKLE_ROOT_BYTES,
    PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES, PREIMAGE_SIZE,
};

fn field_from_le_bytes(bytes: &[u8]) -> F {
    let bytes: &[u8; 32] = bytes
        .try_into()
        .expect("protocol message field slot should be 32 bytes");
    F::from_bytes_le(bytes)
        .into_option()
        .expect("protocol message field slot should be canonical")
}

macro_rules! field_wrapper {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub(crate) struct $name(F);

        impl $name {
            pub(crate) const ZERO: Self = Self(F::ZERO);

            pub(crate) fn new(value: F) -> Self {
                Self(value)
            }

            pub(crate) fn from_field(value: F) -> Self {
                Self(value)
            }

            pub(crate) fn as_field(self) -> F {
                self.0
            }
        }
    };
}

field_wrapper!(MessageHash);
field_wrapper!(MerkleTreeCommitment);
field_wrapper!(ProtocolParametersHash);
field_wrapper!(CertificateCircuitVerificationKeyRepresentation);
field_wrapper!(IvcCircuitVerificationKeyRepresentation);

macro_rules! u64_wrapper {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub(crate) struct $name(u64);

        impl $name {
            pub(crate) const ZERO: Self = Self(0);

            pub(crate) fn new(value: u64) -> Self {
                Self(value)
            }

            pub(crate) fn zero() -> Self {
                Self(0)
            }

            pub(crate) fn as_u64(self) -> u64 {
                self.0
            }

            pub(crate) fn as_field(self) -> F {
                F::from(self.0)
            }

            pub(crate) fn from_field(value: F) -> Self {
                let bytes = value.to_bytes_le();
                let low_bytes = bytes[0..8]
                    .try_into()
                    .expect("field byte representation should contain at least 8 bytes");
                Self(u64::from_le_bytes(low_bytes))
            }
        }

        impl From<u64> for $name {
            fn from(value: u64) -> Self {
                Self(value)
            }
        }

        impl From<$name> for u64 {
            fn from(value: $name) -> Self {
                value.0
            }
        }
    };
}

u64_wrapper!(EpochNumber);
u64_wrapper!(StepCounter);

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ProtocolMessagePreimage([u8; PREIMAGE_SIZE]);

impl ProtocolMessagePreimage {
    pub(crate) fn new(bytes: [u8; PREIMAGE_SIZE]) -> Self {
        Self(bytes)
    }

    pub(crate) fn as_bytes(&self) -> &[u8; PREIMAGE_SIZE] {
        &self.0
    }

    pub(crate) fn as_mut_bytes(&mut self) -> &mut [u8; PREIMAGE_SIZE] {
        &mut self.0
    }

    pub(crate) fn into_inner(self) -> [u8; PREIMAGE_SIZE] {
        self.0
    }

    pub(crate) fn next_merkle_root(&self) -> MerkleTreeCommitment {
        MerkleTreeCommitment::from_field(field_from_le_bytes(
            &self.0[PREIMAGE_NEXT_MERKLE_ROOT_BYTES],
        ))
    }

    pub(crate) fn next_protocol_params(&self) -> ProtocolParametersHash {
        ProtocolParametersHash::from_field(field_from_le_bytes(
            &self.0[PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES],
        ))
    }

    pub(crate) fn current_epoch(&self) -> EpochNumber {
        let bytes = self.0[PREIMAGE_CURRENT_EPOCH_BYTES]
            .try_into()
            .expect("current epoch preimage slot should be 8 bytes");
        EpochNumber::new(u64::from_le_bytes(bytes))
    }
}

impl From<[u8; PREIMAGE_SIZE]> for ProtocolMessagePreimage {
    fn from(bytes: [u8; PREIMAGE_SIZE]) -> Self {
        Self(bytes)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct CertificateProofBytes(Vec<u8>);

impl CertificateProofBytes {
    pub(crate) fn from_snark_proof_bytes(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }

    pub(crate) fn empty() -> Self {
        Self(Vec::new())
    }

    #[cfg(test)]
    pub(crate) fn garbage(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }

    pub(crate) fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    pub(crate) fn into_vec(self) -> Vec<u8> {
        self.0
    }
}

/// Provisional recursive proof-byte wrapper until `IvcProof` is wired end-to-end.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct IvcProofBytes(Vec<u8>);

impl IvcProofBytes {
    pub(crate) fn new(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }

    pub(crate) fn empty() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    pub(crate) fn into_vec(self) -> Vec<u8> {
        self.0
    }
}

impl From<Vec<u8>> for IvcProofBytes {
    fn from(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }
}
