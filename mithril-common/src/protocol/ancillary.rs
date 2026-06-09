//! Building the ancillary proof input fed to aggregate signature creation.

use mithril_stm::{AncillaryGenesisData, AncillaryProofInput};

use crate::entities::Certificate;
#[cfg(feature = "future_snark")]
use crate::entities::CertificateSignature;

/// Build the ancillary proof input for one aggregate signature creation, from the genesis
/// certificate (the genesis data) and the parent certificate (the prover data).
///
/// The genesis message is always carried. Under `future_snark`, the genesis Schnorr signature is
/// carried too when the genesis certificate is dual-signed, and absent for a legacy genesis.
pub fn build_ancillary_proof_input(
    genesis_certificate: &Certificate,
    parent_certificate: &Certificate,
) -> AncillaryProofInput {
    let prover_data = parent_certificate
        .ancillary_prover_data
        .clone()
        .map(|prover_data| prover_data.into_inner());

    #[cfg(feature = "future_snark")]
    let genesis_data = {
        let genesis_message = genesis_certificate.protocol_message.rigid_preimage();
        let genesis_schnorr_signature = match &genesis_certificate.signature {
            CertificateSignature::GenesisDualSignature(_, signature) => Some(*signature),
            _ => None,
        };
        AncillaryGenesisData::new(genesis_message, genesis_schnorr_signature)
    };
    #[cfg(not(feature = "future_snark"))]
    let genesis_data =
        AncillaryGenesisData::new(genesis_certificate.signed_message.as_bytes().to_vec());

    AncillaryProofInput::new(prover_data, genesis_data)
}
