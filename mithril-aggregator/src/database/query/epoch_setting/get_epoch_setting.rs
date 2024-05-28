use anyhow::Context;
use sqlite::Value;

use mithril_common::{entities::Epoch, StdResult};
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::EpochSettingRecord;

/// Simple queries to retrieve [EpochSettingRecord] from the sqlite database.
pub struct GetEpochSettingQuery {
    condition: WhereCondition,
}

impl GetEpochSettingQuery {
    pub fn by_epoch(epoch: Epoch) -> StdResult<Self> {
        let epoch_setting_id: i64 = epoch
            .try_into()
            .with_context(|| format!("Can not convert epoch: '{epoch}'"))?;

        Ok(Self {
            condition: WhereCondition::new(
                "epoch_setting_id = ?*",
                vec![Value::Integer(epoch_setting_id)],
            ),
        })
    }
}

impl Query for GetEpochSettingQuery {
    type Entity = EpochSettingRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:epoch_setting:}", "es")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from epoch_setting as es where {condition} order by epoch_setting_id desc")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::ProtocolParameters;
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::{insert_epoch_settings, main_db_connection};

    use super::*;

    #[test]
    fn test_get_epoch_settings() {
        let connection = main_db_connection().unwrap();
        insert_epoch_settings(&connection, &[1, 2, 3]).unwrap();

        let epoch_setting_record = connection
            .fetch_first(GetEpochSettingQuery::by_epoch(Epoch(1)).unwrap())
            .unwrap()
            .expect("Should have an epoch setting for epoch 1.");
        assert_eq!(Epoch(1), epoch_setting_record.epoch_setting_id);
        assert_eq!(
            ProtocolParameters::new(1, 2, 1.0),
            epoch_setting_record.protocol_parameters
        );

        let epoch_setting_record = connection
            .fetch_first(GetEpochSettingQuery::by_epoch(Epoch(3)).unwrap())
            .unwrap()
            .expect("Should have an epoch setting for epoch 3.");
        assert_eq!(Epoch(3), epoch_setting_record.epoch_setting_id);
        assert_eq!(
            ProtocolParameters::new(3, 4, 1.0),
            epoch_setting_record.protocol_parameters
        );

        let cursor = connection
            .fetch(GetEpochSettingQuery::by_epoch(Epoch(5)).unwrap())
            .unwrap();
        assert_eq!(0, cursor.count());
    }
}
