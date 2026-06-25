//! Building the ancillary proof input fed to aggregate signature creation.

use mithril_stm::{AncillaryGenesisData, AncillaryProofInput};

#[cfg(feature = "future_snark")]
use crate::crypto_helper::GenesisSchnorrVerificationKey;
use crate::entities::Certificate;
#[cfg(feature = "future_snark")]
use crate::entities::{CertificateSignature, ProtocolMessage};

/// Build the ancillary proof input for one aggregate signature creation, from the genesis
/// certificate (the genesis data) and the parent certificate (the prover data).
///
/// Under `future_snark`, the genesis message preimage is carried, the genesis Schnorr signature too
/// when the genesis certificate is dual-signed (absent for a legacy genesis), the genesis Schnorr
/// verification key supplied by the caller from configuration, and the rigid preimage of the
/// protocol message being aggregated.
pub fn build_ancillary_proof_input(
    genesis_certificate: &Certificate,
    parent_certificate: &Certificate,
    #[cfg(feature = "future_snark")] protocol_message: &ProtocolMessage,
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
        let genesis_schnorr_signature = match &genesis_certificate.signature {
            CertificateSignature::GenesisDualSignature(_, signature) => Some(*signature),
            _ => None,
        };
        AncillaryGenesisData::new(
            genesis_message_preimage,
            genesis_schnorr_signature,
            genesis_schnorr_verification_key,
        )
    };
    #[cfg(not(feature = "future_snark"))]
    let genesis_data = {
        let _ = genesis_certificate;
        AncillaryGenesisData::new()
    };

    #[cfg(feature = "future_snark")]
    let message_preimage = protocol_message.rigid_preimage();

    AncillaryProofInput::new(
        prover_data,
        genesis_data,
        #[cfg(feature = "future_snark")]
        message_preimage,
    )
}

#[cfg(all(test, feature = "future_snark"))]
mod tests {
    use std::ops::Deref;

    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::crypto_helper::{GenesisEd25519Signature, GenesisSchnorrSigner};
    use crate::test::double::{fake_data, fake_keys};

    use super::*;

    fn dual_signed_genesis_certificate() -> Certificate {
        let ed_signature: GenesisEd25519Signature =
            fake_keys::genesis_signature()[0].try_into().unwrap();
        let mut rng = ChaCha20Rng::from_seed([9u8; 32]);
        let schnorr_signer = GenesisSchnorrSigner::generate(&mut rng);
        let schnorr_signature = schnorr_signer.sign(&[0u8; 32], &mut rng).unwrap();

        Certificate {
            signature: CertificateSignature::GenesisDualSignature(ed_signature, schnorr_signature),
            ..fake_data::genesis_certificate("genesis")
        }
    }

    #[test]
    fn carries_message_preimage_and_schnorr_signature_for_a_dual_genesis() {
        let parent = fake_data::certificate("parent");
        let genesis = dual_signed_genesis_certificate();
        let protocol_message = ProtocolMessage::new_rigid();
        let schnorr_verification_key =
            GenesisSchnorrSigner::generate(&mut ChaCha20Rng::from_seed([1u8; 32]))
                .verification_key();

        let input = build_ancillary_proof_input(
            &genesis,
            &parent,
            &protocol_message,
            Some(schnorr_verification_key),
        );

        let genesis_data = input.genesis_data();
        assert_eq!(
            input.message_preimage(),
            protocol_message.rigid_preimage().as_slice()
        );
        assert_eq!(
            genesis_data.genesis_message_preimage().deref(),
            genesis.protocol_message.rigid_preimage().as_slice()
        );
        assert!(genesis_data.genesis_schnorr_signature().is_some());
        assert!(genesis_data.genesis_schnorr_verification_key().is_some());
        assert!(input.prover_data().is_none());
    }

    #[test]
    fn omits_the_schnorr_signature_and_key_for_a_legacy_genesis() {
        let parent = fake_data::certificate("parent");
        let genesis = fake_data::genesis_certificate("genesis");
        let protocol_message = ProtocolMessage::new_rigid();

        let input = build_ancillary_proof_input(&genesis, &parent, &protocol_message, None);

        assert!(input.genesis_data().genesis_schnorr_signature().is_none());
        assert!(input.genesis_data().genesis_schnorr_verification_key().is_none());
    }
}
