use crate::runtime::WorkingCertificate;
use chrono::Utc;
use mithril_common::{
    crypto_helper::{key_encode_hex, ProtocolMultiSignature, PROTOCOL_VERSION},
    entities::{self, PartyId},
};
use thiserror::Error;

/// Error type for multi signer service.
#[derive(Error, Debug)]
pub enum CertificateCreationError {
    /// Codec error.
    #[error("codec error: '{0}'")]
    Codec(String),
}

/// Define a way to create a [Certificate]
pub trait CertificateCreator {
    /// Create a [Certificate]
    fn create_certificate(
        working: &WorkingCertificate,
        signatures_party_ids: &[PartyId],
        multi_signature: ProtocolMultiSignature,
    ) -> Result<entities::Certificate, CertificateCreationError>;
}

/// Implementation of a [CertificateCreator]
pub struct MithrilCertificateCreator {}

impl CertificateCreator for MithrilCertificateCreator {
    /// Creates a certificate from a multi signature
    fn create_certificate(
        working_certificate: &WorkingCertificate,
        signatures_party_ids: &[PartyId],
        multi_signature: ProtocolMultiSignature,
    ) -> Result<entities::Certificate, CertificateCreationError> {
        let protocol_version = PROTOCOL_VERSION.to_string();
        let initiated_at = format!("{:?}", working_certificate.initiated_at);
        let sealed_at = format!("{:?}", Utc::now());
        let signers = working_certificate
            .signers
            .iter()
            .filter(|signer| signatures_party_ids.contains(&signer.party_id))
            .cloned()
            .collect::<Vec<_>>();
        let metadata = entities::CertificateMetadata::new(
            protocol_version,
            working_certificate.protocol_parameters.clone(),
            initiated_at,
            sealed_at,
            signers,
        );
        let multi_signature =
            key_encode_hex(multi_signature).map_err(CertificateCreationError::Codec)?;
        let genesis_signature = "".to_string();

        Ok(entities::Certificate::new(
            working_certificate.previous_hash.clone(),
            working_certificate.beacon.clone(),
            metadata,
            working_certificate.message.clone(),
            working_certificate.aggregate_verification_key.clone(),
            multi_signature,
            genesis_signature,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        certificate_creator::MithrilCertificateCreator, runtime::WorkingCertificate,
        CertificateCreator,
    };
    use chrono::{DateTime, Utc};
    use mithril_common::{
        crypto_helper::{key_decode_hex, tests_setup::setup_certificate_chain},
        entities::PartyId,
    };
    use std::str::FromStr;

    #[test]
    fn test() {
        let (certificates, _) = setup_certificate_chain(3, 1);
        let expected = &certificates[1];
        let working_certicate = WorkingCertificate {
            beacon: expected.beacon.clone(),
            protocol_parameters: expected.metadata.protocol_parameters.clone(),
            signers: expected.metadata.signers.clone(),
            message: expected.protocol_message.clone(),
            aggregate_verification_key: expected.aggregate_verification_key.clone(),
            initiated_at: DateTime::<Utc>::from_str(&expected.metadata.initiated_at).unwrap(),
            previous_hash: expected.previous_hash.clone(),
        };
        let party_ids: Vec<PartyId> = expected
            .metadata
            .signers
            .iter()
            .map(|s| s.party_id.clone())
            .collect();

        let mut certificate = MithrilCertificateCreator::create_certificate(
            &working_certicate,
            &party_ids,
            key_decode_hex(&expected.multi_signature.clone()).unwrap(),
        )
        .expect("certificate creation should not fail");
        // Note: We can't sync the 'sealed_at' property with the expected cert before hand since it's
        // computed by create_certificate itself, so we need to do that now :
        certificate.metadata.sealed_at = expected.metadata.sealed_at.clone();
        certificate.hash = certificate.compute_hash();

        assert_eq!(expected, &certificate);
    }
}
