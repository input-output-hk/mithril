#![allow(deprecated)]

use mithril_common::{
    entities::CertificatePending,
    messages::{CertificatePendingMessage, TryFromMessageAdapter},
    StdResult,
};

/// Adapter to turn [CertificatePendingMessage] instances into [CertificatePending].
pub struct FromPendingCertificateMessageAdapter;

impl TryFromMessageAdapter<CertificatePendingMessage, CertificatePending>
    for FromPendingCertificateMessageAdapter
{
    /// Adapter method
    fn try_adapt(message: CertificatePendingMessage) -> StdResult<CertificatePending> {
        let certificate = CertificatePending {
            epoch: message.epoch,
            signed_entity_type: message.signed_entity_type,
            protocol_parameters: message.protocol_parameters,
            next_protocol_parameters: message.next_protocol_parameters,
            // This field is deprecated and should not be used in Signer.
            signers: vec![],
            // This field is deprecated and should not be used in Signer.
            next_signers: vec![],
        };

        Ok(certificate)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adapt_ok() {
        let message = CertificatePendingMessage::dummy();
        let epoch = message.epoch;
        let certificate_pending = FromPendingCertificateMessageAdapter::try_adapt(message).unwrap();

        assert_eq!(epoch, certificate_pending.epoch);
    }
}
