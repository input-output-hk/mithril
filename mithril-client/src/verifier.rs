use hex::ToHex;
use log::debug;
use thiserror::Error;

use mithril_common::crypto_helper::{key_decode_hex, ProtocolMultiSignature};
use mithril_common::entities::ProtocolParameters;

#[cfg(test)]
use mockall::automock;

/// [Verifier::verify_multi_signature] related errors.
#[derive(Error, Debug)]
pub enum ProtocolError {
    /// Error raised when the multi signatures verification fails.
    #[error("multi signature verification failed: '{0}'")]
    VerifyMultiSignature(String),

    /// Error raised when encoding or decoding of data to hex fails.
    #[error("codec error: '{0}'")]
    Codec(String),
}

/// Verifier is the cryptographic engine in charge of verifying multi signatures and certificates
#[cfg_attr(test, automock)]
pub trait Verifier {
    /// Verify a multi signature
    fn verify_multi_signature(
        &self,
        message: &Vec<u8>,
        multi_signature: &str,
        aggregate_verification_key: &str,
        protocol_parameters: &ProtocolParameters,
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
        message: &Vec<u8>,
        multi_signature: &str,
        aggregate_verification_key: &str,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<(), ProtocolError> {
        debug!(
            "Verify multi signature for {:?}",
            message.encode_hex::<String>()
        );
        let multi_signature: ProtocolMultiSignature =
            key_decode_hex(multi_signature).map_err(ProtocolError::Codec)?;
        let aggregate_verification_key =
            key_decode_hex(aggregate_verification_key).map_err(ProtocolError::Codec)?;
        multi_signature
            .verify(
                message,
                &aggregate_verification_key,
                &protocol_parameters.to_owned().into(),
            )
            .map_err(|e| ProtocolError::VerifyMultiSignature(e.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::crypto_helper::tests_setup::*;
    use mithril_common::crypto_helper::{key_encode_hex, ProtocolClerk};

    #[test]
    fn test_multi_signer_multi_signature_ok() {
        let protocol_parameters = setup_protocol_parameters();
        let signers = setup_signers(5);
        let message = setup_message();

        let mut single_signatures = Vec::new();
        signers.iter().for_each(|(_, _, _, protocol_signer, _)| {
            if let Some(signature) = protocol_signer.sign(message.compute_hash().as_bytes()) {
                single_signatures.push(signature);
            }
        });

        let first_signer = &signers.first().unwrap().3;
        let clerk = ProtocolClerk::from_signer(first_signer);
        let aggregate_verification_key = clerk.compute_avk();
        let multi_signature = clerk
            .aggregate(&single_signatures, &message.compute_hash().as_bytes())
            .unwrap();

        let verifier = VerifierImpl::new();
        let protocol_parameters = protocol_parameters.into();
        let message_tampered = message.compute_hash().as_bytes()[1..].to_vec();
        assert!(
            verifier
                .verify_multi_signature(
                    &message_tampered,
                    &key_encode_hex(&multi_signature).unwrap(),
                    &key_encode_hex(&aggregate_verification_key).unwrap(),
                    &protocol_parameters,
                )
                .is_err(),
            "multi signature verification should have failed"
        );
        verifier
            .verify_multi_signature(
                &message.compute_hash().as_bytes().to_vec(),
                &key_encode_hex(&multi_signature).unwrap(),
                &key_encode_hex(&aggregate_verification_key).unwrap(),
                &protocol_parameters,
            )
            .expect("multi signature verification should have succeeded");
    }
}
