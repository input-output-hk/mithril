use mithril_common::{
    entities::{CertificatePending, Signer},
    messages::{CertificatePendingMessage, SignerMessage, TryFromMessageAdapter},
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
            beacon: message.beacon,
            signed_entity_type: message.signed_entity_type,
            protocol_parameters: message.protocol_parameters,
            next_protocol_parameters: message.next_protocol_parameters,
            signers: Self::adapt_signers(message.signers)?,
            next_signers: Self::adapt_signers(message.next_signers)?,
        };

        Ok(certificate)
    }
}

impl FromPendingCertificateMessageAdapter {
    fn adapt_signers(signer_messages: Vec<SignerMessage>) -> StdResult<Vec<Signer>> {
        let mut signers: Vec<Signer> = Vec::new();

        for msg in signer_messages {
            let value = Signer {
                party_id: msg.party_id,
                verification_key: msg.verification_key.try_into()?,
                verification_key_signature: msg.verification_key_signature,
                kes_period: msg.kes_period,
                operational_certificate: msg.operational_certificate,
            };
            signers.push(value);
        }

        Ok(signers)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adapt_ok() {
        let message = CertificatePendingMessage::dummy();
        let epoch = message.beacon.epoch;
        let certificate_pending = FromPendingCertificateMessageAdapter::try_adapt(message).unwrap();

        assert_eq!(epoch, certificate_pending.beacon.epoch);
    }

    #[test]
    fn adapt_signers() {
        let mut message = CertificatePendingMessage::dummy();
        message.signers = vec![SignerMessage::dummy(), SignerMessage::dummy()];
        let certificate_pending = FromPendingCertificateMessageAdapter::try_adapt(message).unwrap();

        assert_eq!(2, certificate_pending.signers.len());
        assert_eq!(1, certificate_pending.next_signers.len());
    }
}
