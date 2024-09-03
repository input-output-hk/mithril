use mithril_common::entities::{EpochSettings, Signer};
use mithril_common::messages::{EpochSettingsMessage, SignerMessagePart, ToMessageAdapter};

/// Adapter to spawn [EpochSettingsMessage] from [EpochSettings] instances.
pub struct ToEpochSettingsMessageAdapter;

impl ToMessageAdapter<EpochSettings, EpochSettingsMessage> for ToEpochSettingsMessageAdapter {
    /// Turn an entity instance into message.
    fn adapt(epoch_settings: EpochSettings) -> EpochSettingsMessage {
        EpochSettingsMessage {
            epoch: epoch_settings.epoch,
            protocol_parameters: epoch_settings.protocol_parameters,
            next_protocol_parameters: epoch_settings.next_protocol_parameters,
            current_signers: SignerMessagePart::from_signers(epoch_settings.current_signers),
            next_signers: SignerMessagePart::from_signers(epoch_settings.next_signers),
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn test_simple_message() {
        let epoch_settings = fake_data::epoch_settings();
        let message = ToEpochSettingsMessageAdapter::adapt(epoch_settings.clone());

        assert_eq!(epoch_settings.epoch, message.epoch);
    }

    #[test]
    fn adapt_signers() {
        let fake_signers = fake_data::signers(5);
        let current_signers = fake_signers[1..3].to_vec();
        let next_signers = fake_signers[2..5].to_vec();
        let epoch_setting = EpochSettings {
            current_signers,
            next_signers,
            ..fake_data::epoch_settings()
        };
        let message = ToEpochSettingsMessageAdapter::adapt(epoch_setting);

        assert_eq!(2, message.current_signers.len());
        assert_eq!(3, message.next_signers.len());
    }
}
