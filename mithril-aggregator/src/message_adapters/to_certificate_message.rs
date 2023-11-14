use mithril_common::entities::Certificate;
use mithril_common::messages::{CertificateMessage, ToMessageAdapter};

/// Adapter to convert [Certificate] to [CertificateMessage] instances
pub struct ToCertificateMessageAdapter;

impl ToMessageAdapter<Certificate, CertificateMessage> for ToCertificateMessageAdapter {
    /// Method to trigger the conversion
    fn adapt(certificate: Certificate) -> CertificateMessage {
        certificate.try_into().unwrap()
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
