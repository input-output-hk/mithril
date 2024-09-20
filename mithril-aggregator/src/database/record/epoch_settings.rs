use mithril_common::entities::{
    BlockNumber, CardanoTransactionsSigningConfig, Epoch, ProtocolParameters,
};
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

/// Settings for an epoch, including the protocol parameters.
#[derive(Debug, PartialEq)]
pub struct EpochSettingsRecord {
    /// Epoch settings id, i.e. the epoch number.
    pub epoch_settings_id: Epoch,

    /// Protocol parameters.
    pub protocol_parameters: ProtocolParameters,

    /// Cardano transactions signing configuration.
    pub cardano_transactions_signing_config: CardanoTransactionsSigningConfig,
}

impl SqLiteEntity for EpochSettingsRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let epoch_settings_id_int = row.read::<i64, _>(0);
        let protocol_parameters_string = &row.read::<&str, _>(1);
        let cardano_transactions_signing_config_string = &row.read::<&str, _>(2);

        let epoch_settings_record = Self {
            epoch_settings_id: Epoch(epoch_settings_id_int.try_into().map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not cast i64 ({epoch_settings_id_int}) to u64. Error: '{e}'"
                ))
            })?),
            protocol_parameters: serde_json::from_str(protocol_parameters_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{protocol_parameters_string}' to ProtocolParameters. Error: {e}"
                    ))
                },
            )?,
            cardano_transactions_signing_config: serde_json::from_str(cardano_transactions_signing_config_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{cardano_transactions_signing_config_string}' to CardanoTransactionsSigningConfig. Error: {e}"
                    ))
                },
            )?,

        };

        Ok(epoch_settings_record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field(
            "epoch_setting_id",
            "{:epoch_setting:}.epoch_setting_id",
            "integer",
        );
        projection.add_field(
            "protocol_parameters",
            "{:epoch_setting:}.protocol_parameters",
            "text",
        );
        projection.add_field(
            "cardano_transactions_signing_config",
            "{:epoch_setting:}.cardano_transactions_signing_config",
            "text",
        );

        projection
    }
}
