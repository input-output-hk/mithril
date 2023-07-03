use mithril_common::entities::Certificate;
use mithril_common::messages::{
    CertificateListItemMessage, CertificateListItemMessageMetadata, CertificateListMessage,
    ToMessageAdapter,
};

/// Adapter to convert a list of [Certificate] to [CertificateListMessage] instances
pub struct ToCertificateListMessageAdapter;

impl ToMessageAdapter<Vec<Certificate>, CertificateListMessage>
    for ToCertificateListMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(certificates: Vec<Certificate>) -> CertificateListMessage {
        certificates
            .into_iter()
            .map(|certificate| CertificateListItemMessage {
                hash: certificate.hash,
                previous_hash: certificate.previous_hash,
                beacon: certificate.beacon,
                metadata: CertificateListItemMessageMetadata {
                    protocol_version: certificate.metadata.protocol_version,
                    protocol_parameters: certificate.metadata.protocol_parameters,
                    initiated_at: certificate.metadata.initiated_at,
                    sealed_at: certificate.metadata.sealed_at,
                    total_signers: certificate.metadata.signers.len(),
                },
                protocol_message: certificate.protocol_message,
                signed_message: certificate.signed_message,
                aggregate_verification_key: certificate.aggregate_verification_key,
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn adapt_ok() {
        let certificate = fake_data::certificate("hash123".to_string());

        let certificate_list_message =
            ToCertificateListMessageAdapter::adapt(vec![certificate.clone()]);
        let certificate_list_message_expected = vec![CertificateListItemMessage {
            hash: certificate.hash,
            previous_hash: certificate.previous_hash,
            beacon: certificate.beacon,
            metadata: CertificateListItemMessageMetadata {
                protocol_version: certificate.metadata.protocol_version,
                protocol_parameters: certificate.metadata.protocol_parameters,
                initiated_at: certificate.metadata.initiated_at,
                sealed_at: certificate.metadata.sealed_at,
                total_signers: certificate.metadata.signers.len(),
            },
            protocol_message: certificate.protocol_message,
            signed_message: certificate.signed_message,
            aggregate_verification_key: certificate.aggregate_verification_key,
        }];

        assert_eq!(certificate_list_message_expected, certificate_list_message);
    }
}
