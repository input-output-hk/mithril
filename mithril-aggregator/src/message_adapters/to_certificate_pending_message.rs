use mithril_common::entities::CardanoDbBeacon;
use mithril_common::{
    entities::{CertificatePending, SignedEntityType},
    messages::{CertificatePendingMessage, SignerMessagePart},
};

/// Adapter to turn [CertificatePending] instances into [CertificatePendingMessage].
pub struct ToCertificatePendingMessageAdapter;

impl ToCertificatePendingMessageAdapter {
    /// Method to trigger the conversion
    pub fn adapt(certificate_pending: CertificatePending) -> CertificatePendingMessage {
        #[allow(deprecated)]
        let beacon = match &certificate_pending.signed_entity_type {
            SignedEntityType::CardanoImmutableFilesFull(beacon) => beacon.clone(),
            _ => CardanoDbBeacon::empty(),
        };

        #[allow(deprecated)]
        CertificatePendingMessage {
            epoch: certificate_pending.epoch,
            beacon: Some(beacon),
            signed_entity_type: certificate_pending.signed_entity_type,
            protocol_parameters: certificate_pending.protocol_parameters,
            next_protocol_parameters: certificate_pending.next_protocol_parameters,
            signers: SignerMessagePart::from_signers(certificate_pending.signers),
            next_signers: SignerMessagePart::from_signers(certificate_pending.next_signers),
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        entities::{Epoch, SignedEntityType},
        test_utils::fake_data,
    };

    use super::*;

    #[test]
    fn adapt_ok() {
        let certificate_pending = fake_data::certificate_pending();
        let epoch = certificate_pending.epoch;
        let message = ToCertificatePendingMessageAdapter::adapt(certificate_pending);

        assert_eq!(epoch, message.epoch);
    }

    #[test]
    fn adapt_on_cardano_immutable_files_full_signed_entity_type_ok() {
        let mut certificate_pending = fake_data::certificate_pending();
        let beacon = fake_data::beacon();
        certificate_pending.signed_entity_type =
            SignedEntityType::CardanoImmutableFilesFull(beacon.clone());

        let message = ToCertificatePendingMessageAdapter::adapt(certificate_pending);

        #[allow(deprecated)]
        let beacon_from_message = message.beacon.unwrap();
        assert_eq!(beacon, beacon_from_message);
    }

    #[test]
    fn adapt_on_other_than_cardano_immutable_files_full_signed_entity_type_ok() {
        let mut certificate_pending = fake_data::certificate_pending();
        let beacon = CardanoDbBeacon::new("", 0, 0);
        certificate_pending.signed_entity_type =
            SignedEntityType::MithrilStakeDistribution(Epoch(15));

        let message = ToCertificatePendingMessageAdapter::adapt(certificate_pending);

        #[allow(deprecated)]
        let beacon_from_message = message.beacon.unwrap();
        assert_eq!(beacon, beacon_from_message);
    }

    #[test]
    #[allow(deprecated)]
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
