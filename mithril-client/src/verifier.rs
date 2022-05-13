use log::debug;
use thiserror::Error;

use mithril_common::crypto_helper::{
    key_decode_hex, Bytes, ProtocolClerk, ProtocolKeyRegistration, ProtocolMultiSignature,
    ProtocolPartyId, ProtocolStake, ProtocolStakeDistribution,
};

use crate::entities;

#[cfg(test)]
use mockall::automock;

#[derive(Error, Debug)]
pub enum ProtocolError {
    #[error("multi signature verification failed")]
    VerifyMultiSignatureError(String),
}

/// Verifier is the cryptographic engine in charge of verifying multi signatures and certificates
#[cfg_attr(test, automock)]
pub trait Verifier {
    /// Verify a multi signature
    fn verify_multi_signature(
        &self,
        message: &Bytes,
        multi_signature: &str,
        signers_with_stakes: &[entities::SignerWithStake],
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<(), ProtocolError>;
}

/// VerifierImpl is an implementation of the Verifier
pub struct VerifierImpl {}

impl VerifierImpl {
    /// VerifierImpl factory
    pub fn new() -> Self {
        debug!("New VerifierImpl created");
        Self {}
    }

    /// Creates a clerk
    pub fn create_clerk(
        &self,
        signers_with_stakes: &[entities::SignerWithStake],
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<ProtocolClerk, String> {
        let stakes = signers_with_stakes
            .iter()
            .map(|signer| {
                (
                    signer.party_id as ProtocolPartyId,
                    signer.stake as ProtocolStake,
                )
            })
            .collect::<ProtocolStakeDistribution>();
        let mut key_registration = ProtocolKeyRegistration::init(&stakes);
        signers_with_stakes.iter().for_each(|signer| {
            if let Ok(verification_key) = key_decode_hex(&signer.verification_key) {
                key_registration
                    .register(signer.party_id as ProtocolPartyId, verification_key)
                    .unwrap();
            }
        });
        let closed_registration = key_registration.close();
        Ok(ProtocolClerk::from_registration(
            protocol_parameters.to_owned().into(),
            closed_registration,
        ))
    }
}

impl Default for VerifierImpl {
    fn default() -> Self {
        Self::new()
    }
}

impl Verifier for VerifierImpl {
    /// Verify a multi signature
    fn verify_multi_signature(
        &self,
        message: &Bytes,
        multi_signature: &str,
        signers_with_stakes: &[entities::SignerWithStake],
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<(), ProtocolError> {
        debug!("Verify multi signature for {:?}", message);
        let clerk = self.create_clerk(signers_with_stakes, protocol_parameters);

        // todo: these two declarations are patches. Probably better ways to do this.
        let avk = clerk
            .as_ref()
            .unwrap()
            .compute_avk();
        let protocol_parameters = ProtocolParameters {
            k: protocol_parameters.k,
            m: protocol_parameters.m,
            phi_f: protocol_parameters.phi_f as f64,
        };
        let multi_signature: ProtocolMultiSignature =
            key_decode_hex(multi_signature).map_err(ProtocolError::VerifyMultiSignatureError)?;
        multi_signature
            .verify(message, &avk, &protocol_parameters)
            .map_err(|e| ProtocolError::VerifyMultiSignatureError(e.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::crypto_helper::key_encode_hex;
    use mithril_common::crypto_helper::tests_setup::*;

    #[test]
    fn test_multi_signer_multi_signature_ok() {
        let protocol_parameters = setup_protocol_parameters();
        let signers = setup_signers(5);
        let message = setup_message();

        let mut single_signatures = Vec::new();
        signers.iter().for_each(|(_, _, _, protocol_signer, _)| {
            for i in 1..=protocol_parameters.m {
                if let Some(signature) = protocol_signer.sign(&message, i) {
                    single_signatures.push(signature);
                }
            }
        });

        let first_signer = &signers.first().unwrap().3;
        let clerk = ProtocolClerk::from_signer(&first_signer);
        let multi_signature = clerk.aggregate(&single_signatures, &message).unwrap();

        let verifier = VerifierImpl::new();
        let protocol_parameters = protocol_parameters.into();
        let signers_with_stakes = signers
            .iter()
            .map(|(party_id, stake, verification_key, _, _)| {
                entities::SignerWithStake::new(
                    *party_id as u64,
                    key_encode_hex(verification_key).unwrap(),
                    *stake as u64,
                )
            })
            .collect::<Vec<entities::SignerWithStake>>();
        let message_tampered = message[1..].to_vec();
        assert!(
            verifier
                .verify_multi_signature(
                    &message_tampered,
                    &key_encode_hex(&multi_signature).unwrap(),
                    &signers_with_stakes,
                    &protocol_parameters,
                )
                .is_err(),
            "multi signature verification should have failed"
        );
        verifier
            .verify_multi_signature(
                &message,
                &key_encode_hex(&multi_signature).unwrap(),
                &signers_with_stakes,
                &protocol_parameters,
            )
            .expect("multi signature verification should have succeeded");
    }
}
