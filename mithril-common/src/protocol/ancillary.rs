//! Building the ancillary proof input fed to aggregate signature creation.

use mithril_stm::{AncillaryGenesisData, AncillaryProofInput};

#[cfg(feature = "future_snark")]
use crate::crypto_helper::GenesisSchnorrVerificationKey;
use crate::entities::Certificate;
#[cfg(feature = "future_snark")]
use crate::entities::CertificateSignature;

/// Build the ancillary proof input for one aggregate signature creation, from the genesis
/// certificate (the genesis data) and the parent certificate (the prover data).
///
/// Under `future_snark`, the genesis message preimage is carried, the genesis Schnorr signature too
/// when the genesis certificate is dual-signed (absent for a legacy genesis), and the genesis
/// Schnorr verification key supplied by the caller from configuration.
pub fn build_ancillary_proof_input(
    genesis_certificate: &Certificate,
    parent_certificate: &Certificate,
    #[cfg(feature = "future_snark")] genesis_schnorr_verification_key: Option<
        GenesisSchnorrVerificationKey,
    >,
) -> AncillaryProofInput {
    let prover_data = parent_certificate
        .ancillary_prover_data
        .clone()
        .map(|prover_data| prover_data.into_inner());

    #[cfg(feature = "future_snark")]
    let genesis_data = {
        let genesis_message_preimage = genesis_certificate.protocol_message.rigid_preimage();
        let genesis_message =
            ProtocolMessage::compute_rigid_hash_bytes_from_preimage(&genesis_message_preimage);
        let genesis_schnorr_signature = match &genesis_certificate.signature {
            CertificateSignature::GenesisDualSignature(_, signature) => Some(*signature),
            _ => None,
        };
        AncillaryGenesisData::new(
            genesis_message_preimage,
            genesis_message,
            genesis_schnorr_signature,
            genesis_schnorr_verification_key,
        )
    };
    #[cfg(not(feature = "future_snark"))]
    let genesis_data = {
        let _ = genesis_certificate;
        AncillaryGenesisData::new()
    };

    AncillaryProofInput::new(prover_data, genesis_data)
}
