use sqlite::Value;

use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::EpochSettingsRecord;

/// Query to insert a [EpochSettingsRecord] into the sqlite database if it does not already exist.
pub struct InsertOrIgnoreEpochSettingsQuery {
    condition: WhereCondition,
}

impl InsertOrIgnoreEpochSettingsQuery {
    pub fn one(epoch_settings: EpochSettingsRecord) -> Self {
        Self {
            condition: WhereCondition::new(
                r#"(epoch_setting_id, protocol_parameters, 
                cardano_transactions_signing_config, 
                cardano_blocks_transactions_signing_config) 
                values (?1, ?2, ?3, ?4)"#,
                vec![
                    Value::Integer(*epoch_settings.epoch_settings_id as i64),
                    Value::String(
                        serde_json::to_string(&epoch_settings.protocol_parameters).unwrap(),
                    ),
                    match &epoch_settings.cardano_transactions_signing_config {
                        Some(config) => Value::String(serde_json::to_string(config).unwrap()),
                        None => Value::Null,
                    },
                    match &epoch_settings.cardano_blocks_transactions_signing_config {
                        Some(config) => Value::String(serde_json::to_string(config).unwrap()),
                        None => Value::Null,
                    },
                ],
            ),
        }
    }
}

impl Query for InsertOrIgnoreEpochSettingsQuery {
    type Entity = EpochSettingsRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:epoch_setting:}", "epoch_setting")]));

        format!("insert or ignore into epoch_setting {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{
        BlockNumber, CardanoBlocksTransactionsSigningConfig, CardanoTransactionsSigningConfig,
        Epoch,
    };
    use mithril_common::test::double::fake_data;
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::query::GetEpochSettingsQuery;
    use crate::database::test_helper::main_db_connection;

    use super::*;

    #[test]
    fn test_insert_epoch_setting_in_empty_db() {
        let connection = main_db_connection().unwrap();

        let expected_epoch_settings = EpochSettingsRecord {
            epoch_settings_id: Epoch(3),
            protocol_parameters: fake_data::protocol_parameters(),
            cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(24),
                step: BlockNumber(62),
            }),
            cardano_blocks_transactions_signing_config: Some(
                CardanoBlocksTransactionsSigningConfig {
                    security_parameter: BlockNumber(48),
                    step: BlockNumber(96),
                },
            ),
        };
        let record = connection
            .fetch_first(InsertOrIgnoreEpochSettingsQuery::one(
                expected_epoch_settings.clone(),
            ))
            .unwrap();

        assert_eq!(Some(expected_epoch_settings), record);

        let expected_epoch_settings_without_ctx_config = EpochSettingsRecord {
            epoch_settings_id: Epoch(4),
            protocol_parameters: fake_data::protocol_parameters(),
            cardano_transactions_signing_config: None,
            cardano_blocks_transactions_signing_config: None,
        };
        let record = connection
            .fetch_first(InsertOrIgnoreEpochSettingsQuery::one(
                expected_epoch_settings_without_ctx_config.clone(),
            ))
            .unwrap();

        assert_eq!(Some(expected_epoch_settings_without_ctx_config), record);
    }

    #[test]
    fn test_cant_replace_existing_value() {
        let connection = main_db_connection().unwrap();

        let expected_epoch_settings = EpochSettingsRecord {
            epoch_settings_id: Epoch(3),
            protocol_parameters: fake_data::protocol_parameters(),
            cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(24),
                step: BlockNumber(62),
            }),
            cardano_blocks_transactions_signing_config: Some(
                CardanoBlocksTransactionsSigningConfig {
                    security_parameter: BlockNumber(48),
                    step: BlockNumber(96),
                },
            ),
        };
        let record = connection
            .fetch_first(InsertOrIgnoreEpochSettingsQuery::one(
                expected_epoch_settings.clone(),
            ))
            .unwrap();
        assert!(record.is_some());

        let record = connection
            .fetch_first(InsertOrIgnoreEpochSettingsQuery::one(EpochSettingsRecord {
                cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig {
                    security_parameter: BlockNumber(134),
                    step: BlockNumber(872),
                }),
                cardano_blocks_transactions_signing_config: Some(
                    CardanoBlocksTransactionsSigningConfig {
                        security_parameter: BlockNumber(321),
                        step: BlockNumber(987),
                    },
                ),
                ..expected_epoch_settings.clone()
            }))
            .unwrap();
        assert!(record.is_none());

        let record_in_db = connection
            .fetch_first(GetEpochSettingsQuery::by_epoch(Epoch(3)).unwrap())
            .unwrap();
        assert_eq!(Some(expected_epoch_settings), record_in_db);
    }
}
