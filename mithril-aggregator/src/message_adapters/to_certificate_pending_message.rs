use mithril_common::entities::{CardanoDbBeacon, ImmutableFileNumber};
use mithril_common::{
    entities::{CertificatePending, Signer},
    messages::{CertificatePendingMessage, SignerMessagePart},
    CardanoNetwork,
};

/// Adapter to turn [CertificatePending] instances into [CertificatePendingMessage].
pub struct ToCertificatePendingMessageAdapter;

impl ToCertificatePendingMessageAdapter {
    /// Method to trigger the conversion
    pub fn adapt(
        certificate_pending: CertificatePending,
        network: CardanoNetwork,
        immutable_file_number: ImmutableFileNumber,
    ) -> CertificatePendingMessage {
        let beacon = CardanoDbBeacon::new(
            network.to_string(),
            *certificate_pending.epoch,
            immutable_file_number,
        );

        #[allow(deprecated)]
        CertificatePendingMessage {
            epoch: beacon.epoch,
            beacon,
            signed_entity_type: certificate_pending.signed_entity_type,
            protocol_parameters: certificate_pending.protocol_parameters,
            next_protocol_parameters: certificate_pending.next_protocol_parameters,
            signers: Self::adapt_signers(certificate_pending.signers),
            next_signers: Self::adapt_signers(certificate_pending.next_signers),
        }
    }
}

impl ToCertificatePendingMessageAdapter {
    fn adapt_signers(signers: Vec<Signer>) -> Vec<SignerMessagePart> {
        signers
            .into_iter()
            .map(|signer| SignerMessagePart {
                party_id: signer.party_id,
                verification_key: signer.verification_key.try_into().unwrap(),
                verification_key_signature: signer
                    .verification_key_signature
                    .map(|k| k.try_into().unwrap()),
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
        let epoch = certificate_pending.epoch;
        let message = ToCertificatePendingMessageAdapter::adapt(
            certificate_pending,
            fake_data::network(),
            10,
        );

        assert_eq!(epoch, message.epoch);
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
        let message = ToCertificatePendingMessageAdapter::adapt(
            certificate_pending,
            fake_data::network(),
            10,
        );

        assert_eq!(2, message.signers.len());
        assert_eq!(3, message.next_signers.len());
    }
}
