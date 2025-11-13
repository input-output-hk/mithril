use anyhow::Context;
use mithril_common::{
    StdResult,
    messages::{EpochSettingsMessage, SignerMessagePart, TryFromMessageAdapter},
};

use crate::entities::RegisteredSigners;

/// Adapter to convert [EpochSettingsMessage] to [RegisteredSigners].
pub struct FromEpochSettingsAdapter;

impl TryFromMessageAdapter<EpochSettingsMessage, RegisteredSigners> for FromEpochSettingsAdapter {
    /// Method to convert.
    fn try_adapt(message: EpochSettingsMessage) -> StdResult<RegisteredSigners> {
        let epoch_settings = RegisteredSigners {
            epoch: message.epoch,
            current_signers: SignerMessagePart::try_into_signers(message.current_signers)
                .with_context(|| "'FromMessageAdapter' can not convert the current signers")?,
            next_signers: SignerMessagePart::try_into_signers(message.next_signers)
                .with_context(|| "'FromMessageAdapter' can not convert the next signers")?,
        };
        Ok(epoch_settings)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test::double::Dummy;

    use super::*;

    #[test]
    fn try_adapt_epoch_settings_message_to_entity() {
        let message = EpochSettingsMessage::dummy();
        let epoch = message.epoch;
        let epoch_settings = FromEpochSettingsAdapter::try_adapt(message).unwrap();

        assert_eq!(epoch, epoch_settings.epoch);
    }
}
