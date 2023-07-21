use mithril_common::{
    crypto_helper::ProtocolSignerVerificationKey,
    entities::{CertificatePending, Signer},
    messages::{CertificatePendingMessage, SignerMessage, TryFromMessageAdapter},
    StdResult,
};

/// Adapter to turn [CertificatePendingMessage] instances into [CertificatePending].
pub struct FromPendingCertificateMessageAdapter;

fn to_signers(messages: &[SignerMessage]) -> StdResult<Vec<Signer>> {
    let mut signers: Vec<Signer> = Vec::new();

    for msg in messages {
        let signer = Signer::new(
            msg.party_id.to_owned(),
            ProtocolSignerVerificationKey::from_json_hex(&msg.verification_key)?,
            msg.verification_key_signature.to_owned(),
            msg.operational_certificate.to_owned(),
            msg.kes_period,
        );
        signers.push(signer);
    }

    Ok(signers)
}

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
            signers: to_signers(&message.signers)?,
            next_signers: to_signers(&message.next_signers)?,
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
