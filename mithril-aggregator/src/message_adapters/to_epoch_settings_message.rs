use mithril_common::entities::EpochSettings;
use mithril_common::messages::{EpochSettingsMessage, ToMessageAdapter};

/// Adapter to spawn [EpochSettingsMessage] from [EpochSettings] instances.
pub struct ToEpochSettingsMessageAdapter;

impl ToMessageAdapter<EpochSettings, EpochSettingsMessage> for ToEpochSettingsMessageAdapter {
    /// Turn an entity instance into message.
    fn adapt(epoch_settings: EpochSettings) -> EpochSettingsMessage {
        EpochSettingsMessage {
            epoch: epoch_settings.epoch,
            protocol_parameters: epoch_settings.protocol_parameters,
            next_protocol_parameters: epoch_settings.next_protocol_parameters,
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
}
