use anyhow::Context;
use mithril_common::StdResult;
use mithril_common::entities::Epoch;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};
use sqlite::Value;

use crate::database::record::EpochSettingsRecord;

/// Simple queries to retrieve [EpochSettingsRecord] from the sqlite database.
pub struct GetEpochSettingsQuery {
    condition: WhereCondition,
}

impl GetEpochSettingsQuery {
    pub fn by_epoch(epoch: Epoch) -> StdResult<Self> {
        let epoch_settings_id: i64 = epoch
            .try_into()
            .with_context(|| format!("Can not convert epoch: '{epoch}'"))?;

        Ok(Self {
            condition: WhereCondition::new(
                "epoch_setting_id = ?*",
                vec![Value::Integer(epoch_settings_id)],
            ),
        })
    }
}

impl Query for GetEpochSettingsQuery {
    type Entity = EpochSettingsRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:epoch_setting:}", "es")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!(
            "select {projection} from epoch_setting as es where {condition} order by epoch_setting_id desc"
        )
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{
        BlockNumber, CardanoTransactionsSigningConfig, ProtocolParameters,
    };
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::{insert_epoch_settings, main_db_connection};

    use super::*;

    #[test]
    fn test_get_epoch_settings() {
        let connection = main_db_connection().unwrap();
        insert_epoch_settings(&connection, &[1, 2, 3]).unwrap();

        let epoch_settings_record = connection
            .fetch_first(GetEpochSettingsQuery::by_epoch(Epoch(1)).unwrap())
            .unwrap()
            .expect("Should have an epoch settings for epoch 1.");
        assert_eq!(Epoch(1), epoch_settings_record.epoch_settings_id);
        assert_eq!(
            ProtocolParameters::new(1, 2, 1.0),
            epoch_settings_record.protocol_parameters
        );

        assert_eq!(
            CardanoTransactionsSigningConfig::new(BlockNumber(10), BlockNumber(15)),
            epoch_settings_record.cardano_transactions_signing_config
        );

        let epoch_settings_record = connection
            .fetch_first(GetEpochSettingsQuery::by_epoch(Epoch(3)).unwrap())
            .unwrap()
            .expect("Should have an epoch settings for epoch 3.");
        assert_eq!(Epoch(3), epoch_settings_record.epoch_settings_id);
        assert_eq!(
            ProtocolParameters::new(3, 4, 1.0),
            epoch_settings_record.protocol_parameters
        );
        assert_eq!(
            CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(30),
                step: BlockNumber(15),
            },
            epoch_settings_record.cardano_transactions_signing_config
        );

        let cursor = connection
            .fetch(GetEpochSettingsQuery::by_epoch(Epoch(5)).unwrap())
            .unwrap();
        assert_eq!(0, cursor.count());
    }
}
