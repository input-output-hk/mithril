//! Ancillary data carried by a certificate for the prover and the verifier.
//!
//! These are proof-system-agnostic carriers whose variants map to the aggregate signature types
//! that need them. They start with no variants and grow as those schemes land.

use serde::{Deserialize, Serialize};

use crate::StmResult;
use crate::codec;

/// Ancillary data carried by a certificate for the prover.
///
/// Holds the prover-side state needed to produce the next certificate. Carried in the certificate,
/// hashed into the certificate hash and transmitted in the certificate message. Variants map to the
/// aggregate signature types that require prover data; none exist yet.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum AncillaryProverData {}

impl AncillaryProverData {
    /// Serialize to versioned CBOR bytes, following `CODEC.md`.
    pub fn to_bytes(&self) -> StmResult<Vec<u8>> {
        codec::to_cbor_bytes(self)
    }

    /// Deserialize from versioned CBOR bytes, following `CODEC.md`.
    ///
    /// With no variants this always fails; it gains meaning once a variant is added.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        if codec::has_cbor_v1_prefix(bytes) {
            codec::from_cbor_bytes(&bytes[1..])
        } else {
            Err(anyhow::anyhow!(
                "AncillaryProverData: unsupported encoding, expected a CBOR v1 prefix"
            ))
        }
    }
}

/// Ancillary data carried by a certificate for the verifier.
///
/// Holds the data a verifier needs to verify the certificate. Stored and transmitted in the
/// certificate message. Variants map to the aggregate signature types that require verifier
/// data; none exist yet.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum AncillaryVerifierData {}

impl AncillaryVerifierData {
    /// Serialize to versioned CBOR bytes, following `CODEC.md`.
    pub fn to_bytes(&self) -> StmResult<Vec<u8>> {
        codec::to_cbor_bytes(self)
    }

    /// Deserialize from versioned CBOR bytes, following `CODEC.md`.
    ///
    /// With no variants this always fails; it gains meaning once a variant is added.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        if codec::has_cbor_v1_prefix(bytes) {
            codec::from_cbor_bytes(&bytes[1..])
        } else {
            Err(anyhow::anyhow!(
                "AncillaryVerifierData: unsupported encoding, expected a CBOR v1 prefix"
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codec::CODEC_VERSION_CBOR_V1;

    use super::*;

    #[test]
    fn prover_data_from_bytes_rejects_every_input() {
        assert!(AncillaryProverData::from_bytes(&[]).is_err());
        assert!(AncillaryProverData::from_bytes(&[0, 1, 2]).is_err());
        assert!(AncillaryProverData::from_bytes(&[CODEC_VERSION_CBOR_V1, 0xff]).is_err());
    }

    #[test]
    fn verifier_data_from_bytes_rejects_every_input() {
        assert!(AncillaryVerifierData::from_bytes(&[]).is_err());
        assert!(AncillaryVerifierData::from_bytes(&[0, 1, 2]).is_err());
        assert!(AncillaryVerifierData::from_bytes(&[CODEC_VERSION_CBOR_V1, 0xff]).is_err());
    }
}
