use mithril_common::entities::Certificate;
use mithril_common::messages::CertificateMessage;

/// Adapter to convert [CertificateMessage] to [Certificate] instances
pub struct FromCertificateMessageAdapter;

impl FromCertificateMessageAdapter {
    /// Method to trigger the conversion
    pub fn adapt(certificate_message: CertificateMessage) -> Certificate {
        Certificate {
            hash: certificate_message.hash,
            previous_hash: certificate_message.previous_hash,
            beacon: certificate_message.beacon,
            metadata: certificate_message.metadata,
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
    use mithril_common::entities::{
        Beacon, CertificateMetadata, ProtocolMessage, ProtocolParameters, SignerWithStake,
    };

    use either::Either;

    use super::*;

    #[test]
    fn adapt_ok() {
        let certificate_message = CertificateMessage {
            hash: "hash123".to_string(),
            previous_hash: "previous_hash-modified".to_string(),
            beacon: Beacon::new("testnet".to_string(), 10, 100),
            metadata: CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new(
                        "1".to_string(),
                        "verification-key-123".to_string(),
                        None,
                        None,
                        None,
                        10,
                    ),
                    SignerWithStake::new(
                        "2".to_string(),
                        "verification-key-456".to_string(),
                        None,
                        None,
                        None,
                        20,
                    ),
                ],
            ),
            protocol_message: Either::Left(ProtocolMessage::new()),
            signed_message: "signed_message".to_string(),
            aggregate_verification_key: "aggregate_verification_key".to_string(),
            multi_signature: "multi_signature".to_string(),
            genesis_signature: "genesis_signature".to_string(),
        };
        let certificate = FromCertificateMessageAdapter::adapt(certificate_message);

        assert_eq!("hash123".to_string(), certificate.hash);
    }
}
