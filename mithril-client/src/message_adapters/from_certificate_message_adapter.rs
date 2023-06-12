use mithril_common::entities::{Certificate, CertificateMetadata};
use mithril_common::messages::CertificateMessage;

/// Adapter to convert [CertificateMessage] to [Certificate] instances
pub struct FromCertificateMessageAdapter;

impl FromCertificateMessageAdapter {
    /// Method to trigger the conversion
    pub fn adapt(certificate_message: CertificateMessage) -> Certificate {
        let metadata = CertificateMetadata {
            protocol_version: certificate_message.metadata.protocol_version,
            protocol_parameters: certificate_message.metadata.protocol_parameters,
            initiated_at: certificate_message.metadata.initiated_at,
            sealed_at: certificate_message.metadata.sealed_at,
            signers: certificate_message.metadata.signers,
        };

        Certificate {
            hash: certificate_message.hash,
            previous_hash: certificate_message.previous_hash,
            beacon: certificate_message.beacon,
            metadata,
            protocol_message: certificate_message.protocol_message,
            signed_message: certificate_message.signed_message,
            aggregate_verification_key: certificate_message.aggregate_verification_key,
            multi_signature: certificate_message.multi_signature,
            genesis_signature: certificate_message.genesis_signature,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adapt_ok() {
        let certificate_message = CertificateMessage {
            hash: "hash123".to_string(),
            ..Default::default()
        };
        let certificate = FromCertificateMessageAdapter::adapt(certificate_message);

        assert_eq!("hash123".to_string(), certificate.hash);
    }
}
