//! Ancillary data carried by a certificate for the prover and the verifier.
//!
//! These are proof-system-agnostic carriers whose variants map to the aggregate signature types
//! that need them. They start with no variants and grow as those schemes land.

use serde::{Deserialize, Serialize};

use crate::StmResult;
use crate::codec;
use crate::proof_system::IvcRollingState;
use crate::proof_system::ivc_halo2_snark::verifier_setup::IvcVerifierData;
#[cfg(feature = "future_snark")]
use crate::{SchnorrVerificationKey, StandardSchnorrSignature};

/// Ancillary data carried by a certificate for the prover.
///
/// Holds the prover-side state needed to produce the next certificate. Carried in the certificate,
/// hashed into the certificate hash and transmitted in the certificate message. Variants map to the
/// aggregate signature types that require prover data; none exist yet.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum AncillaryProverData {
    IvcSnark(IvcRollingState),
}

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

    pub fn as_ivc_rolling_state(&self) -> IvcRollingState {
        match self {
            Self::IvcSnark(state) => state.clone(),
        }
    }
}

/// Ancillary data carried by a certificate for the verifier.
///
/// Holds the data a verifier needs to verify the certificate. Stored and transmitted in the
/// certificate message. Variants map to the aggregate signature types that require verifier
/// data; none exist yet.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum AncillaryVerifierData {
    IvcSnark(IvcVerifierData),
}

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

    pub fn as_ivc_verifier_data(&self) -> IvcVerifierData {
        match self {
            Self::IvcSnark(state) => state.clone(),
        }
    }
}

/// Genesis-related data carried into aggregate signature creation.
///
/// Under `future_snark`, holds the genesis message preimage, the genesis Schnorr signature and the
/// genesis Schnorr verification key when the genesis certificate carries them. It is a transient
/// input to proof creation, never stored on a certificate.
#[derive(Clone, Debug)]
pub struct AncillaryGenesisData {
    #[cfg(feature = "future_snark")]
    genesis_message_preimage: Vec<u8>,
    #[cfg(feature = "future_snark")]
    genesis_schnorr_signature: Option<StandardSchnorrSignature>,
    #[cfg(feature = "future_snark")]
    genesis_schnorr_verification_key: Option<SchnorrVerificationKey>,
}

impl AncillaryGenesisData {
    /// Build the genesis ancillary data. Under `future_snark`, from the genesis message preimage,
    /// the genesis Schnorr signature and the genesis Schnorr verification key (the signature absent
    /// for a legacy, non-dual genesis certificate).
    #[cfg_attr(not(feature = "future_snark"), allow(clippy::new_without_default))]
    pub fn new(
        #[cfg(feature = "future_snark")] genesis_message_preimage: Vec<u8>,
        #[cfg(feature = "future_snark")] genesis_schnorr_signature: Option<
            StandardSchnorrSignature,
        >,
        #[cfg(feature = "future_snark")] genesis_schnorr_verification_key: Option<
            SchnorrVerificationKey,
        >,
    ) -> Self {
        Self {
            #[cfg(feature = "future_snark")]
            genesis_message_preimage,
            #[cfg(feature = "future_snark")]
            genesis_schnorr_signature,
            #[cfg(feature = "future_snark")]
            genesis_schnorr_verification_key,
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

    /// Return the genesis Schnorr verification key, absent for a legacy (non-dual) genesis
    /// certificate.
    #[cfg(feature = "future_snark")]
    pub fn genesis_schnorr_verification_key(&self) -> Option<&SchnorrVerificationKey> {
        self.genesis_schnorr_verification_key.as_ref()
    }

    /// Build genesis ancillary data carrying no data, for use in tests.
    #[cfg(test)]
    pub fn dummy() -> Self {
        Self::new(
            #[cfg(feature = "future_snark")]
            Vec::new(),
            #[cfg(feature = "future_snark")]
            None,
            #[cfg(feature = "future_snark")]
            None,
        )
    }
}

/// Ancillary input to one aggregate signature creation.
///
/// Carries the prover data from the previous certificate, the genesis data from the genesis
/// certificate and, under `future_snark`, the rigid preimage of the protocol message being
/// aggregated, the state the proof system needs at creation. It is always supplied to the clerk;
/// each proof system decides whether to consume it.
#[derive(Clone, Debug)]
pub struct AncillaryProofInput {
    prover_data: Option<AncillaryProverData>,
    genesis_data: AncillaryGenesisData,
    #[cfg(feature = "future_snark")]
    message_preimage: Vec<u8>,
}

impl AncillaryProofInput {
    /// Build the ancillary proof input from the prover data, the genesis data and, under
    /// `future_snark`, the rigid preimage of the protocol message being aggregated.
    pub fn new(
        prover_data: Option<AncillaryProverData>,
        genesis_data: AncillaryGenesisData,
        #[cfg(feature = "future_snark")] message_preimage: Vec<u8>,
    ) -> Self {
        Self {
            prover_data,
            genesis_data,
            #[cfg(feature = "future_snark")]
            message_preimage,
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

    /// Return the rigid preimage of the protocol message being aggregated.
    #[cfg(feature = "future_snark")]
    pub fn message_preimage(&self) -> &[u8] {
        &self.message_preimage
    }

    /// Build an ancillary proof input carrying no data, for use in tests.
    #[cfg(test)]
    pub fn dummy() -> Self {
        Self::new(
            None,
            AncillaryGenesisData::dummy(),
            #[cfg(feature = "future_snark")]
            Vec::new(),
        )
    }
}

/// Ancillary output from one aggregate signature creation.
///
/// Carries the prover data to store on the new certificate (the state the proof system needs to
/// produce the next certificate) and the verifier data the new certificate exposes. It is the
/// output counterpart of [`AncillaryProofInput`]; each proof system decides whether to produce
/// either, so both are absent for a proof system that produces none.
#[derive(Clone, Debug)]
pub struct AncillaryProofOutput {
    prover_data: Option<AncillaryProverData>,
    verifier_data: Option<AncillaryVerifierData>,
}

impl AncillaryProofOutput {
    /// Build the ancillary proof output from the prover and verifier data.
    pub fn new(
        prover_data: Option<AncillaryProverData>,
        verifier_data: Option<AncillaryVerifierData>,
    ) -> Self {
        Self {
            prover_data,
            verifier_data,
        }
    }

    /// Return the prover ancillary data to store on the new certificate.
    pub fn prover_data(&self) -> Option<&AncillaryProverData> {
        self.prover_data.as_ref()
    }

    /// Return the verifier ancillary data the new certificate exposes.
    pub fn verifier_data(&self) -> Option<&AncillaryVerifierData> {
        self.verifier_data.as_ref()
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

    #[test]
    fn proof_output_exposes_the_data_it_was_built_with() {
        let output = AncillaryProofOutput::new(None, None);

        assert!(output.prover_data().is_none());
        assert!(output.verifier_data().is_none());
    }

    #[cfg(feature = "future_snark")]
    #[test]
    fn genesis_data_getters_return_the_values_it_was_built_with() {
        let preimage = vec![1u8, 2, 3];

        let genesis_data = AncillaryGenesisData::new(preimage.clone(), None, None);

        assert_eq!(genesis_data.genesis_message_preimage(), preimage.as_slice());
        assert!(genesis_data.genesis_schnorr_signature().is_none());
        assert!(genesis_data.genesis_schnorr_verification_key().is_none());
    }

    #[cfg(feature = "future_snark")]
    #[test]
    fn proof_input_returns_the_message_preimage_it_was_built_with() {
        let message_preimage = vec![9u8, 8, 7];

        let proof_input = AncillaryProofInput::new(
            None,
            AncillaryGenesisData::dummy(),
            message_preimage.clone(),
        );

        assert_eq!(proof_input.message_preimage(), message_preimage.as_slice());
    }
}
