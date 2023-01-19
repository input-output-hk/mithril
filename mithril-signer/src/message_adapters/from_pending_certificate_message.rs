use mithril_common::{entities::CertificatePending, messages::CertificatePendingMessage};

/// Adapter to turn [CertificatePendingMessage] instances into [CertificatePending].
pub struct FromPendingCertificateMessageAdapter;

impl FromPendingCertificateMessageAdapter {
    /// Adapter method
    pub fn adapt(message: CertificatePendingMessage) -> CertificatePending {
        CertificatePending {
            beacon: message.beacon,
            protocol_parameters: message.protocol_parameters,
            next_protocol_parameters: message.next_protocol_parameters,
            signers: message.signers,
            next_signers: message.next_signers,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adapt_ok() {
        let message = CertificatePendingMessage::dummy();
        let epoch = message.beacon.epoch;
        let certificate_pending = FromPendingCertificateMessageAdapter::adapt(message);

        assert_eq!(epoch, certificate_pending.beacon.epoch);
    }
}
