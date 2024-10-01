use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::EpochSettingsRecord;
use crate::entities::AggregatorEpochSettings;

/// Query to update [EpochSettingsRecord] in the sqlite database
pub struct UpdateEpochSettingsQuery {
    condition: WhereCondition,
}

impl UpdateEpochSettingsQuery {
    pub fn one(epoch: Epoch, epoch_settings: AggregatorEpochSettings) -> Self {
        let epoch_settings_id: i64 = epoch.try_into().unwrap();

        Self {
            condition: WhereCondition::new(
                "(epoch_setting_id, protocol_parameters, cardano_transactions_signing_config) values (?1, ?2, ?3)",
                vec![
                    Value::Integer(epoch_settings_id),
                    Value::String(serde_json::to_string(&epoch_settings.protocol_parameters).unwrap()),
                    Value::String(
                        serde_json::to_string(&epoch_settings.cardano_transactions_signing_config).unwrap(),
                    ),
                ],
            ),
        }
    }
}

impl Query for UpdateEpochSettingsQuery {
    type Entity = EpochSettingsRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:epoch_setting:}", "epoch_setting")]));

        format!("insert or replace into epoch_setting {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{BlockNumber, CardanoTransactionsSigningConfig};
    use mithril_common::test_utils::fake_data;
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::query::GetEpochSettingsQuery;
    use crate::database::test_helper::{insert_epoch_settings, main_db_connection};

    use super::*;

    #[test]
    fn test_update_epoch_settings() {
        let connection = main_db_connection().unwrap();
        insert_epoch_settings(&connection, &[*Epoch(3)]).unwrap();

        let epoch_settings_send_to_update = AggregatorEpochSettings {
            protocol_parameters: fake_data::protocol_parameters(),
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig::new(
                BlockNumber(24),
                BlockNumber(62),
            ),
        };
        let record_returned_by_update_query = connection
            .fetch_first(UpdateEpochSettingsQuery::one(
                Epoch(3),
                epoch_settings_send_to_update.clone(),
            ))
            .unwrap()
            .unwrap();

        assert_eq!(Epoch(3), record_returned_by_update_query.epoch_settings_id);
        assert_eq!(
            epoch_settings_send_to_update.protocol_parameters,
            record_returned_by_update_query.protocol_parameters
        );
        assert_eq!(
            epoch_settings_send_to_update.cardano_transactions_signing_config,
            record_returned_by_update_query.cardano_transactions_signing_config
        );

        let mut cursor = connection
            .fetch(GetEpochSettingsQuery::by_epoch(Epoch(3)).unwrap())
            .unwrap();
        let epoch_settings_record = cursor
            .next()
            .expect("Should have an epoch settings for epoch 3.");

        assert_eq!(record_returned_by_update_query, epoch_settings_record);
        assert_eq!(0, cursor.count());
    }
}
