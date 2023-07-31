use mithril_common::entities::{Certificate, CertificateSignature};
use mithril_common::messages::{
    CertificateMessage, CertificateMetadataMessage, SignerWithStakeMessagePart, ToMessageAdapter,
};

/// Adapter to convert [Certificate] to [CertificateMessage] instances
pub struct ToCertificateMessageAdapter;

impl ToMessageAdapter<Certificate, CertificateMessage> for ToCertificateMessageAdapter {
    /// Method to trigger the conversion
    fn adapt(certificate: Certificate) -> CertificateMessage {
        let metadata = CertificateMetadataMessage {
            protocol_version: certificate.metadata.protocol_version,
            protocol_parameters: certificate.metadata.protocol_parameters,
            initiated_at: certificate.metadata.initiated_at,
            sealed_at: certificate.metadata.sealed_at,
            signers: SignerWithStakeMessagePart::from_signers(certificate.metadata.signers),
        };

        let (multi_signature, genesis_signature) = match certificate.signature {
            CertificateSignature::GenesisSignature(signature) => (String::new(), signature),
            CertificateSignature::MultiSignature(signature) => {
                (signature.to_json_hex().unwrap(), String::new())
            }
        };

        CertificateMessage {
            hash: certificate.hash,
            previous_hash: certificate.previous_hash,
            beacon: certificate.beacon,
            metadata,
            protocol_message: certificate.protocol_message,
            signed_message: certificate.signed_message,
            aggregate_verification_key: certificate.aggregate_verification_key,
            multi_signature,
            genesis_signature,
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
