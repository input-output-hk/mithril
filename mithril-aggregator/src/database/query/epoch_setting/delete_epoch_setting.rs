use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::EpochSettingRecord;

/// Query to delete old [EpochSettingRecord] from the sqlite database
pub struct DeleteEpochSettingQuery {
    condition: WhereCondition,
}

impl Query for DeleteEpochSettingQuery {
    type Entity = EpochSettingRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:epoch_setting:}", "epoch_setting")]));

        format!("delete from epoch_setting where {condition} returning {projection}")
    }
}

impl DeleteEpochSettingQuery {
    #[cfg(test)]
    /// Create the SQL condition to delete a record given the Epoch.
    pub fn by_epoch(epoch: Epoch) -> Self {
        let epoch_setting_id_value = Value::Integer(epoch.try_into().unwrap());

        Self {
            condition: WhereCondition::new("epoch_setting_id = ?*", vec![epoch_setting_id_value]),
        }
    }

    /// Create the SQL condition to prune data older than the given Epoch.
    pub fn below_epoch_threshold(epoch_threshold: Epoch) -> Self {
        let epoch_setting_id_value = Value::Integer(epoch_threshold.try_into().unwrap());

        Self {
            condition: WhereCondition::new("epoch_setting_id < ?*", vec![epoch_setting_id_value]),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::database::query::GetEpochSettingQuery;
    use crate::database::test_helper::{insert_epoch_settings, main_db_connection};
    use mithril_persistence::sqlite::ConnectionExtensions;

    use super::*;

    #[test]
    fn test_delete_by_epoch() {
        let connection = main_db_connection().unwrap();
        insert_epoch_settings(&connection, &[1, 2]).unwrap();

        let cursor = connection
            .fetch(DeleteEpochSettingQuery::by_epoch(Epoch(2)))
            .unwrap();

        assert_eq!(1, cursor.count());

        let cursor = connection
            .fetch(GetEpochSettingQuery::by_epoch(Epoch(1)).unwrap())
            .unwrap();

        assert_eq!(1, cursor.count());

        let cursor = connection
            .fetch(GetEpochSettingQuery::by_epoch(Epoch(2)).unwrap())
            .unwrap();

        assert_eq!(0, cursor.count());
    }

    #[test]
    fn test_delete_below_threshold() {
        let connection = main_db_connection().unwrap();
        insert_epoch_settings(&connection, &[1, 2]).unwrap();

        let cursor = connection
            .fetch(DeleteEpochSettingQuery::below_epoch_threshold(Epoch(2)))
            .unwrap();

        assert_eq!(1, cursor.count());

        let cursor = connection
            .fetch(GetEpochSettingQuery::by_epoch(Epoch(1)).unwrap())
            .unwrap();

        assert_eq!(0, cursor.count());

        let cursor = connection
            .fetch(GetEpochSettingQuery::by_epoch(Epoch(2)).unwrap())
            .unwrap();

        assert_eq!(1, cursor.count());
    }
}
