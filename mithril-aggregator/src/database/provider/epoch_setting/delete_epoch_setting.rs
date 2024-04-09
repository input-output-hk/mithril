use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::EpochSettingRecord;

/// Query to delete old [EpochSettingRecord] from the sqlite database
pub struct DeleteEpochSettingProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> Provider<'conn> for DeleteEpochSettingProvider<'conn> {
    type Entity = EpochSettingRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:epoch_setting:}", "epoch_setting")]));

        format!("delete from epoch_setting where {condition} returning {projection}")
    }
}

impl<'conn> DeleteEpochSettingProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    #[cfg(test)]
    /// Create the SQL condition to delete a record given the Epoch.
    fn get_delete_condition_by_epoch(&self, epoch: Epoch) -> WhereCondition {
        let epoch_setting_id_value = Value::Integer(epoch.try_into().unwrap());

        WhereCondition::new("epoch_setting_id = ?*", vec![epoch_setting_id_value])
    }

    #[cfg(test)]
    /// Delete the epoch setting data given the Epoch
    pub fn delete(&self, epoch: Epoch) -> StdResult<EntityCursor<EpochSettingRecord>> {
        let filters = self.get_delete_condition_by_epoch(epoch);

        self.find(filters)
    }

    /// Create the SQL condition to prune data older than the given Epoch.
    fn get_prune_condition(&self, epoch_threshold: Epoch) -> WhereCondition {
        let epoch_setting_id_value = Value::Integer(epoch_threshold.try_into().unwrap());

        WhereCondition::new("epoch_setting_id < ?*", vec![epoch_setting_id_value])
    }

    /// Prune the epoch setting data older than the given epoch.
    pub fn prune(&self, epoch_threshold: Epoch) -> StdResult<EntityCursor<EpochSettingRecord>> {
        let filters = self.get_prune_condition(epoch_threshold);

        self.find(filters)
    }
}

#[cfg(test)]
mod tests {
    use crate::database::provider::GetEpochSettingProvider;
    use crate::database::test_helper::{insert_epoch_settings, main_db_connection};

    use super::*;

    #[test]
    fn test_delete() {
        let connection = main_db_connection().unwrap();
        insert_epoch_settings(&connection, &[1, 2]).unwrap();

        let provider = DeleteEpochSettingProvider::new(&connection);
        let cursor = provider.delete(Epoch(2)).unwrap();

        assert_eq!(1, cursor.count());

        let provider = GetEpochSettingProvider::new(&connection);
        let cursor = provider.get_by_epoch(&Epoch(1)).unwrap();

        assert_eq!(1, cursor.count());

        let cursor = provider.get_by_epoch(&Epoch(2)).unwrap();

        assert_eq!(0, cursor.count());
    }

    #[test]
    fn test_prune() {
        let connection = main_db_connection().unwrap();
        insert_epoch_settings(&connection, &[1, 2]).unwrap();

        let provider = DeleteEpochSettingProvider::new(&connection);
        let cursor = provider.prune(Epoch(2)).unwrap();

        assert_eq!(1, cursor.count());

        let provider = GetEpochSettingProvider::new(&connection);
        let cursor = provider.get_by_epoch(&Epoch(1)).unwrap();

        assert_eq!(0, cursor.count());

        let cursor = provider.get_by_epoch(&Epoch(2)).unwrap();

        assert_eq!(1, cursor.count());
    }
}
