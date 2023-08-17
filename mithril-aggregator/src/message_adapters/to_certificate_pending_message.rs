use mithril_common::{
    entities::{CertificatePending, Signer},
    messages::{CertificatePendingMessage, SignerMessage, ToMessageAdapter},
};

/// Adapter to turn [CertificatePending] instances into [CertificatePendingMessage].
pub struct ToCertificatePendingMessageAdapter;

impl ToMessageAdapter<CertificatePending, CertificatePendingMessage>
    for ToCertificatePendingMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(certificate_pending: CertificatePending) -> CertificatePendingMessage {
        CertificatePendingMessage {
            beacon: certificate_pending.beacon,
            signed_entity_type: certificate_pending.signed_entity_type,
            protocol_parameters: certificate_pending.protocol_parameters,
            next_protocol_parameters: certificate_pending.next_protocol_parameters,
            signers: Self::adapt_signers(certificate_pending.signers),
            next_signers: Self::adapt_signers(certificate_pending.next_signers),
        }
    }
}

impl ToCertificatePendingMessageAdapter {
    fn adapt_signers(signers: Vec<Signer>) -> Vec<SignerMessage> {
        signers
            .into_iter()
            .map(|signer| SignerMessage {
                party_id: signer.party_id,
                verification_key: signer.verification_key.try_into().unwrap(),
                verification_key_signature: signer.verification_key_signature,
                kes_period: signer.kes_period,
                operational_certificate: signer
                    .operational_certificate
                    .map(|o| o.try_into().unwrap()),
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
        let certificate_pending = fake_data::certificate_pending();
        let epoch = certificate_pending.beacon.epoch;
        let message = ToCertificatePendingMessageAdapter::adapt(certificate_pending);

        assert_eq!(epoch, message.beacon.epoch);
    }

    #[test]
    fn adapt_signers() {
        let fake_signers = fake_data::signers(5);
        let signers = fake_signers[1..3].to_vec();
        let next_signers = fake_signers[2..5].to_vec();
        let certificate_pending = CertificatePending {
            signers,
            next_signers,
            ..fake_data::certificate_pending()
        };
        let message = ToCertificatePendingMessageAdapter::adapt(certificate_pending);

        assert_eq!(2, message.signers.len());
        assert_eq!(3, message.next_signers.len());
    }
}
