use mithril_common::entities::Certificate;
use mithril_common::messages::CertificateMessage;

/// Adapter to convert [Certificate] to [CertificateMessage] instances
pub struct ToCertificateMessageAdapter;

impl ToCertificateMessageAdapter {
    /// Method to trigger the conversion
    pub fn adapt(certificate: Certificate) -> CertificateMessage {
        CertificateMessage {
            hash: certificate.hash,
            previous_hash: certificate.previous_hash,
            beacon: certificate.beacon,
            metadata: certificate.metadata,
            protocol_message: certificate.protocol_message,
            signed_message: certificate.signed_message,
            aggregate_verification_key: certificate.aggregate_verification_key,
            multi_signature: certificate.multi_signature,
            genesis_signature: certificate.genesis_signature,
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn adapt_ok() {
        let certificate = fake_data::certificate("hash123".to_string());
        let certificate_message = ToCertificateMessageAdapter::adapt(certificate);

        assert_eq!("hash123".to_string(), certificate_message.hash);
    }
}
