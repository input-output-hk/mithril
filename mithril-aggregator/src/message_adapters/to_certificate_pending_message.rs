use mithril_common::{entities::CertificatePending, messages::CertificatePendingMessage};

/// Adapter to turn [CertificatePending] instances into [CertificatePendingMessage].
pub struct ToCertificatePendingMessageAdapter;

impl ToCertificatePendingMessageAdapter {
    pub fn adapt(certificate_pending: CertificatePending) -> CertificatePendingMessage {
        CertificatePendingMessage {
            beacon: certificate_pending.beacon,
            protocol_parameters: certificate_pending.protocol_parameters,
            next_protocol_parameters: certificate_pending.next_protocol_parameters,
            signers: certificate_pending.signers,
            next_signers: certificate_pending.next_signers,
        }
    }
}
#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn adapt_ok() {
        let certificate_pending = fake_data::certificate_pending();
        let epoch = certificate_pending.beacon.epoch;
        let message = ToCertificatePendingMessageAdapter::adapt(certificate_pending);

        assert_eq!(epoch, message.beacon.epoch);
    }
}
