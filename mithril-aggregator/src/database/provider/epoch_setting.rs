use anyhow::Context;
use sqlite::Value;

use mithril_common::{
    entities::{Epoch, ProtocolParameters},
    StdResult,
};
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::EpochSettingRecord;

/// Simple queries to retrieve [EpochSettingRecord] from the sqlite database.
pub(crate) struct EpochSettingProvider<'client> {
    client: &'client SqliteConnection,
}

impl<'client> EpochSettingProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client SqliteConnection) -> Self {
        Self { client }
    }

    fn condition_by_epoch(&self, epoch: &Epoch) -> StdResult<WhereCondition> {
        let epoch_setting_id: i64 = epoch
            .try_into()
            .with_context(|| format!("Can not convert epoch: '{epoch}'"))?;

        Ok(WhereCondition::new(
            "epoch_setting_id = ?*",
            vec![Value::Integer(epoch_setting_id)],
        ))
    }

    /// Get EpochSettingRecords for a given Epoch for given pool_ids.
    pub fn get_by_epoch(&self, epoch: &Epoch) -> StdResult<EntityCursor<EpochSettingRecord>> {
        let filters = self.condition_by_epoch(epoch)?;
        let epoch_setting_record = self.find(filters)?;

        Ok(epoch_setting_record)
    }
}

impl<'client> Provider<'client> for EpochSettingProvider<'client> {
    type Entity = EpochSettingRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:epoch_setting:}", "es")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from epoch_setting as es where {condition} order by epoch_setting_id desc")
    }
}

/// Query to update [EpochSettingRecord] in the sqlite database
pub(crate) struct UpdateEpochSettingProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> UpdateEpochSettingProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    // todo: not pub
    pub fn get_update_condition(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> WhereCondition {
        let epoch_setting_id: i64 = epoch.try_into().unwrap();

        WhereCondition::new(
            "(epoch_setting_id, protocol_parameters) values (?1, ?2)",
            vec![
                Value::Integer(epoch_setting_id),
                Value::String(serde_json::to_string(&protocol_parameters).unwrap()),
            ],
        )
    }

    pub fn persist(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> StdResult<EpochSettingRecord> {
        let filters = self.get_update_condition(epoch, protocol_parameters);

        let entity = self
            .find(filters)?
            .next()
            .unwrap_or_else(|| panic!("No entity returned by the persister, epoch = {epoch:?}"));

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for UpdateEpochSettingProvider<'conn> {
    type Entity = EpochSettingRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:epoch_setting:}", "epoch_setting")]));

        format!("insert or replace into epoch_setting {condition} returning {projection}")
    }
}

/// Query to delete old [EpochSettingRecord] from the sqlite database
pub(crate) struct DeleteEpochSettingProvider<'conn> {
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
    use sqlite::Connection;

    use mithril_common::test_utils::fake_data;

    use crate::database::test_helper::{apply_all_migrations_to_db, insert_epoch_settings};

    use super::*;

    pub fn setup_epoch_setting_db(
        connection: &SqliteConnection,
        epoch_to_insert_settings: &[u64],
    ) -> StdResult<()> {
        apply_all_migrations_to_db(connection)?;
        insert_epoch_settings(connection, epoch_to_insert_settings)?;
        Ok(())
    }

    #[test]
    fn projection() {
        let projection = EpochSettingRecord::get_projection();
        let aliases = SourceAlias::new(&[("{:epoch_setting:}", "es")]);

        assert_eq!(
            "es.epoch_setting_id as epoch_setting_id, es.protocol_parameters as protocol_parameters".to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn get_epoch_setting_by_epoch() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = EpochSettingProvider::new(&connection);
        let condition = provider.condition_by_epoch(&Epoch(17)).unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("epoch_setting_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::Integer(17)], values);
    }

    #[test]
    fn update_epoch_setting() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = UpdateEpochSettingProvider::new(&connection);
        let condition = provider.get_update_condition(Epoch(1), ProtocolParameters::new(1, 2, 1.0));
        let (values, params) = condition.expand();

        assert_eq!(
            "(epoch_setting_id, protocol_parameters) values (?1, ?2)".to_string(),
            values
        );
        assert_eq!(
            vec![
                Value::Integer(1),
                Value::String(serde_json::to_string(&ProtocolParameters::new(1, 2, 1.0)).unwrap())
            ],
            params
        );
    }

    #[test]
    fn delete() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = DeleteEpochSettingProvider::new(&connection);
        let condition = provider.get_delete_condition_by_epoch(Epoch(5));
        let (condition, params) = condition.expand();

        assert_eq!("epoch_setting_id = ?1".to_string(), condition);
        assert_eq!(vec![Value::Integer(5)], params);
    }

    #[test]
    fn prune() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = DeleteEpochSettingProvider::new(&connection);
        let condition = provider.get_prune_condition(Epoch(5));
        let (condition, params) = condition.expand();

        assert_eq!("epoch_setting_id < ?1".to_string(), condition);
        assert_eq!(vec![Value::Integer(5)], params);
    }

    #[test]
    fn test_get_epoch_settings() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_epoch_setting_db(&connection, &[1, 2, 3]).unwrap();

        let provider = EpochSettingProvider::new(&connection);

        let mut cursor = provider.get_by_epoch(&Epoch(1)).unwrap();
        let epoch_setting_record = cursor
            .next()
            .expect("Should have an epoch setting for epoch 1.");
        assert_eq!(Epoch(1), epoch_setting_record.epoch_setting_id);
        assert_eq!(
            ProtocolParameters::new(1, 2, 1.0),
            epoch_setting_record.protocol_parameters
        );

        let mut cursor = provider.get_by_epoch(&Epoch(3)).unwrap();
        let epoch_setting_record = cursor
            .next()
            .expect("Should have an epoch setting for epoch 3.");
        assert_eq!(Epoch(3), epoch_setting_record.epoch_setting_id);
        assert_eq!(
            ProtocolParameters::new(3, 4, 1.0),
            epoch_setting_record.protocol_parameters
        );

        let cursor = provider.get_by_epoch(&Epoch(5)).unwrap();
        assert_eq!(0, cursor.count());
    }

    #[test]
    fn test_update_epoch_setting() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_epoch_setting_db(&connection, &[3]).unwrap();

        let provider = UpdateEpochSettingProvider::new(&connection);
        let epoch_setting_record = provider
            .persist(Epoch(3), fake_data::protocol_parameters())
            .unwrap();

        assert_eq!(Epoch(3), epoch_setting_record.epoch_setting_id);
        assert_eq!(
            fake_data::protocol_parameters(),
            epoch_setting_record.protocol_parameters
        );

        let provider = EpochSettingProvider::new(&connection);
        let mut cursor = provider.get_by_epoch(&Epoch(3)).unwrap();
        let epoch_setting_record = cursor
            .next()
            .expect("Should have an epoch setting for epoch 3.");

        assert_eq!(Epoch(3), epoch_setting_record.epoch_setting_id);
        assert_eq!(
            fake_data::protocol_parameters(),
            epoch_setting_record.protocol_parameters
        );
        assert_eq!(0, cursor.count());
    }

    #[test]
    fn test_delete() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_epoch_setting_db(&connection, &[1, 2]).unwrap();

        let provider = DeleteEpochSettingProvider::new(&connection);
        let cursor = provider.delete(Epoch(2)).unwrap();

        assert_eq!(1, cursor.count());

        let provider = EpochSettingProvider::new(&connection);
        let cursor = provider.get_by_epoch(&Epoch(1)).unwrap();

        assert_eq!(1, cursor.count());

        let cursor = provider.get_by_epoch(&Epoch(2)).unwrap();

        assert_eq!(0, cursor.count());
    }

    #[test]
    fn test_prune() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_epoch_setting_db(&connection, &[1, 2]).unwrap();

        let provider = DeleteEpochSettingProvider::new(&connection);
        let cursor = provider.prune(Epoch(2)).unwrap();

        assert_eq!(1, cursor.count());

        let provider = EpochSettingProvider::new(&connection);
        let cursor = provider.get_by_epoch(&Epoch(1)).unwrap();

        assert_eq!(0, cursor.count());

        let cursor = provider.get_by_epoch(&Epoch(2)).unwrap();

        assert_eq!(1, cursor.count());
    }
}
