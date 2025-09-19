use anyhow::Context;
use mithril_common::{
    StdResult,
    messages::{EpochSettingsMessage, SignerMessagePart, TryFromMessageAdapter},
};

use crate::entities::SignerEpochSettings;

/// Adapter to convert [EpochSettingsMessage] to [SignerEpochSettings].
pub struct FromEpochSettingsAdapter;

impl TryFromMessageAdapter<EpochSettingsMessage, SignerEpochSettings> for FromEpochSettingsAdapter {
    /// Method to convert.
    fn try_adapt(message: EpochSettingsMessage) -> StdResult<SignerEpochSettings> {
        let epoch_settings = SignerEpochSettings {
            epoch: message.epoch,
            registration_protocol_parameters: message.signer_registration_protocol_parameters,
            current_signers: SignerMessagePart::try_into_signers(message.current_signers)
                .with_context(|| "'FromMessageAdapter' can not convert the current signers")?,
            next_signers: SignerMessagePart::try_into_signers(message.next_signers)
                .with_context(|| "'FromMessageAdapter' can not convert the next signers")?,
            cardano_transactions_signing_config: message.cardano_transactions_signing_config,
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
