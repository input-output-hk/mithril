use mithril_common::entities::{Certificate, CertificateMetadata, CertificateSignature};
use mithril_common::messages::{
    CertificateMessage, SignerWithStakeMessagePart, TryFromMessageAdapter,
};
use mithril_common::StdResult;

/// Adapter to convert [CertificateMessage] to [Certificate] instances
pub struct FromCertificateMessageAdapter;

impl TryFromMessageAdapter<CertificateMessage, Certificate> for FromCertificateMessageAdapter {
    /// Method to trigger the conversion
    fn try_adapt(certificate_message: CertificateMessage) -> StdResult<Certificate> {
        let metadata = CertificateMetadata {
            protocol_version: certificate_message.metadata.protocol_version,
            protocol_parameters: certificate_message.metadata.protocol_parameters,
            initiated_at: certificate_message.metadata.initiated_at,
            sealed_at: certificate_message.metadata.sealed_at,
            signers: SignerWithStakeMessagePart::try_into_signers(
                certificate_message.metadata.signers,
            )?,
        };

        let certificate = Certificate {
            hash: certificate_message.hash,
            previous_hash: certificate_message.previous_hash,
            beacon: certificate_message.beacon,
            metadata,
            protocol_message: certificate_message.protocol_message,
            signed_message: certificate_message.signed_message,
            aggregate_verification_key: certificate_message
                .aggregate_verification_key
                .try_into()?,
            signature: if certificate_message.genesis_signature.is_empty() {
                CertificateSignature::MultiSignature(
                    certificate_message.multi_signature.try_into()?,
                )
            } else {
                CertificateSignature::GenesisSignature(
                    certificate_message.genesis_signature.try_into()?,
                )
            },
        };

        Ok(certificate)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adapt_ok() {
        let certificate_message = CertificateMessage {
            hash: "hash123".to_string(),
            ..CertificateMessage::dummy()
        };
        let certificate = FromCertificateMessageAdapter::try_adapt(certificate_message).unwrap();

        assert_eq!("hash123".to_string(), certificate.hash);
    }
}
