//! Ancillary data carried by a certificate for the prover and the verifier.
//!
//! These are proof-system-agnostic carriers whose variants map to the aggregate signature types
//! that need them. They start with no variants and grow as those schemes land.

use serde::{Deserialize, Serialize};

#[cfg(feature = "future_snark")]
use crate::StandardSchnorrSignature;
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

/// Genesis-related data carried into aggregate signature creation.
///
/// Under `future_snark`, holds the genesis message preimage and the genesis Schnorr signature when
/// the genesis certificate carries one. It is a transient input to proof creation, never stored on
/// a certificate.
#[derive(Clone, Debug)]
pub struct AncillaryGenesisData {
    #[cfg(feature = "future_snark")]
    genesis_message_preimage: Vec<u8>,
    #[cfg(feature = "future_snark")]
    genesis_schnorr_signature: Option<StandardSchnorrSignature>,
}

impl AncillaryGenesisData {
    /// Build the genesis ancillary data. Under `future_snark`, from the genesis message preimage and
    /// the genesis Schnorr signature (absent for a legacy, non-dual genesis certificate).
    #[cfg_attr(not(feature = "future_snark"), allow(clippy::new_without_default))]
    pub fn new(
        #[cfg(feature = "future_snark")] genesis_message_preimage: Vec<u8>,
        #[cfg(feature = "future_snark")] genesis_schnorr_signature: Option<
            StandardSchnorrSignature,
        >,
    ) -> Self {
        Self {
            #[cfg(feature = "future_snark")]
            genesis_message_preimage,
            #[cfg(feature = "future_snark")]
            genesis_schnorr_signature,
        }
    }

    /// Return the genesis message preimage.
    #[cfg(feature = "future_snark")]
    pub fn genesis_message_preimage(&self) -> &[u8] {
        &self.genesis_message_preimage
    }

    /// Return the genesis Schnorr signature, absent for a legacy (non-dual) genesis certificate.
    #[cfg(feature = "future_snark")]
    pub fn genesis_schnorr_signature(&self) -> Option<&StandardSchnorrSignature> {
        self.genesis_schnorr_signature.as_ref()
    }

    /// Build genesis ancillary data carrying no data, for use in tests.
    #[cfg(test)]
    pub fn dummy() -> Self {
        Self::new(
            #[cfg(feature = "future_snark")]
            Vec::new(),
            #[cfg(feature = "future_snark")]
            None,
        )
    }
}

/// Ancillary input to one aggregate signature creation.
///
/// Carries the prover data from the previous certificate and the genesis data from the genesis
/// certificate, the state the proof system needs at creation. It is always supplied to the clerk;
/// each proof system decides whether to consume it.
#[derive(Clone, Debug)]
pub struct AncillaryProofInput {
    prover_data: Option<AncillaryProverData>,
    genesis_data: AncillaryGenesisData,
}

impl AncillaryProofInput {
    /// Build the ancillary proof input from the prover and genesis data.
    pub fn new(
        prover_data: Option<AncillaryProverData>,
        genesis_data: AncillaryGenesisData,
    ) -> Self {
        Self {
            prover_data,
            genesis_data,
        }
    }

    /// Return the prover ancillary data carried from the previous certificate.
    pub fn prover_data(&self) -> Option<&AncillaryProverData> {
        self.prover_data.as_ref()
    }

    /// Return the genesis ancillary data.
    pub fn genesis_data(&self) -> &AncillaryGenesisData {
        &self.genesis_data
    }

    /// Build an ancillary proof input carrying no data, for use in tests.
    #[cfg(test)]
    pub fn dummy() -> Self {
        Self::new(None, AncillaryGenesisData::dummy())
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
